{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.JavaScript where

import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline as UnsafeC
import Foreign.C.String
import Foreign.Ptr
import Foreign.StablePtr
import Data.Text (Text)
import qualified Data.Text as T
import Data.Int
import Data.Word
import Language.Haskell.TH.Quote
import System.Environment
import Language.JavaScript.Internal
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Data.Coerce
import Data.IORef
import Data.Proxy
import qualified Data.Vector.Storable.Mutable as V
import qualified Data.Vector.Storable as SV
import Data.Kind
import GHC.TypeLits
import GHC.Generics
import Data.Typeable
import Data.Map (Map)

data V8Platform
data Isolate
data Script
data ObjectTemplate
data V8Value
data Global (t :: Type)
data Context

C.context $ v8Ctx
  [ ("IsolatePtr", [t|Ptr Isolate|])
  , ("ScriptPtr", [t|Ptr Script|])
  , ("ObjectTemplatePtr", [t|Ptr ObjectTemplate|])
  , ("GlobalObjectTemplatePtr", [t|Ptr (Global ObjectTemplate)|])
  , ("ContextPtr", [t|Ptr Context|])
  , ("GlobalContextPtr", [t|Ptr (Global Context)|])
  , ("ValuePtr", [t|Ptr V8Value|])
  , ("GlobalValuePtr", [t|Ptr (Global V8Value)|])
  , ("PlatformPtr", [t|Ptr V8Platform|])
  ]

C.include "<iostream>"
C.include "<exception>"
C.include "<stdio.h>"
C.include "<stdlib.h>"
C.include "<string.h>"
C.include "<map>"
C.include "libplatform/libplatform.h"
C.include "v8.h"
C.include "HsV8.hpp"

C.verbatim "typedef void* IsolatePtr;"
C.verbatim "typedef void* ScriptPtr;"
C.verbatim "typedef void* ObjectTemplatePtr;"
C.verbatim "typedef void* ContextPtr;"
C.verbatim "typedef void* GlobalContextPtr;"
C.verbatim "typedef void* ValuePtr;"
C.verbatim "typedef void* GlobalValuePtr;"
C.verbatim "typedef void* PlatformPtr;"
C.verbatim "typedef void* GlobalObjectTemplatePtr;"


data JSException = JSException
  deriving (Show, Eq)

data JSValue t = JSValue {runJSValue :: Ptr Isolate -> Ptr V8Value -> IO (Either JSException t)}
  deriving (Functor)

instance Applicative JSValue where
  pure a = JSValue $ \_ _ -> pure $ Right a
  f <*> a = JSValue $ \iso val -> do
    funE <- runJSValue f iso val
    valE <- runJSValue a iso val
    pure (funE <*> valE)
      

class InterpretJS t where
  jsValue :: JSValue t

instance InterpretJS Int32 where
  jsValue = JSValue $ \isolate v8Val -> do
    res <- [C.block|int32_t {
      v8::Value* value = static_cast<v8::Value*>($(ValuePtr v8Val));
      v8::Isolate* isolate = static_cast<v8::Isolate*>($(IsolatePtr isolate));
      v8::Local<v8::Context> context(isolate->GetCurrentContext());
      
      auto res = value->Int32Value(context);
      if(res.IsJust()) {
        return res.FromJust();
      } else {
        printf("NaN\n");
        return 0;
      }
    }|]
    pure $ Right $ coerce res

instance InterpretJS Int64 where
  jsValue = JSValue $ \isolate v8Val -> do
    res <- [C.block|int64_t {
      v8::Value* value = static_cast<v8::Value*>($(ValuePtr v8Val));
      v8::Isolate* isolate = static_cast<v8::Isolate*>($(IsolatePtr isolate));
      v8::Local<v8::Context> context(isolate->GetCurrentContext());

      
      auto res = value->ToBigInt(context).ToLocalChecked()->Int64Value();
      return res;
    }|]
    pure $ Right $ coerce res

instance InterpretJS Word32 where
  jsValue = JSValue $ \isolate v8Val -> do
    res <- [C.block|uint32_t {
      v8::Value* value = static_cast<v8::Value*>($(ValuePtr v8Val));
      v8::Isolate* isolate = static_cast<v8::Isolate*>($(IsolatePtr isolate));
      v8::Local<v8::Context> context(isolate->GetCurrentContext());
      
      auto res = value->Uint32Value(context);
      if(res.IsJust()) {
        return res.FromJust();
      } else {
        printf("NaN\n");
        return 0;
      }
    }|]
    pure $ Right $ coerce res

instance (InterpretJS a, InterpretJS b) => InterpretJS (a, b) where
  jsValue = JSValue $ \isolate v8Val -> do
    arrLen <- [UnsafeC.block|int32_t {
      v8::Value* value = static_cast<v8::Value*>($(ValuePtr v8Val));
      if(value->IsArray()){
        v8::Array* array = v8::Array::Cast(value);
        return (array->Length());
      } else {
        return -1;
      }
    }|]

    case arrLen == 2 of
      False -> pure $ Left undefined
      True -> do
        arrMV <- V.new (fromIntegral arrLen)
        V.unsafeWith arrMV $ \(arrMVPtr :: Ptr (Ptr V8Value)) -> [C.block| void {
          v8::Value* value = static_cast<v8::Value*>($(ValuePtr v8Val));
          v8::Array* array = v8::Array::Cast(value);
          ValuePtr* valArr = $(ValuePtr* arrMVPtr);
          for(int i = 0; i < $(int32_t arrLen); i++) {
           v8::Local<v8::Value> elem = array->Get(i);
           valArr[i] = *elem;
          }
         }|]
        arr <- SV.unsafeFreeze arrMV
        a <- runJSValue (jsValue :: JSValue a) isolate (arr SV.! 0)
        b <- runJSValue (jsValue :: JSValue b) isolate (arr SV.! 1)
        pure $ (,) <$> a <*> b

instance (SV.Storable t, InterpretJS t) => InterpretJS (SV.Vector t) where
  jsValue = JSValue $ \isolate v8Val -> do
    arrLen <- [UnsafeC.block|int32_t {
      v8::Value* value = static_cast<v8::Value*>($(ValuePtr v8Val));
      if(value->IsArray()){
        v8::Array* array = v8::Array::Cast(value);
        return (array->Length());
      } else {
        return -1;
      }
    }|]

    case arrLen == 0 of
      True -> pure $ Right SV.empty
      False -> do
        arrMV <- V.new (fromIntegral arrLen)
        V.unsafeWith arrMV $ \(arrMVPtr :: Ptr (Ptr V8Value)) -> [C.block| void {
          v8::Value* value = static_cast<v8::Value*>($(ValuePtr v8Val));
          v8::Array* array = v8::Array::Cast(value);
          ValuePtr* valArr = $(ValuePtr* arrMVPtr);
          for(int i = 0; i < $(int32_t arrLen); i++) {
           v8::Local<v8::Value> elem = array->Get(i);
           valArr[i] = *elem;
          }
         }|]
        arr <- SV.unsafeFreeze arrMV
        Right <$> (SV.forM arr $ \v -> do
          Right r <- runJSValue (jsValue :: JSValue t) isolate v
          pure r)  

instance InterpretJS Text where
  jsValue = (error "TODO:")


instance (InjectJS a, InterpretJS b, MkJSFun b (IsFun b)) => InterpretJS (a -> b) where
  jsValue = JSValue $ \isolate v8Val -> do
    isFun <- [UnsafeC.block| int {
        v8::Value* value = static_cast<v8::Value*>($(ValuePtr v8Val));
        
        if(value->IsFunction()){
          return true;
        } else {
          return false;
        }
       }|]

    funcContextPtr <- [UnsafeC.block|GlobalContextPtr {
        v8::Isolate* isolate = static_cast<v8::Isolate*>($(IsolatePtr isolate));
        v8::Local<v8::Context> context(isolate->GetCurrentContext());
        if (context.IsEmpty()) {
         fprintf(stderr, "Error creating context..........\n");
        } else {
          fprintf(stderr, "Success creating context..........\n");
        }
        v8::Global<v8::Context>* funcCtx = new v8::Global<v8::Context>(isolate, context);

        return funcCtx;
      }|]

    pure $ Right $ mkJSFun isolate funcContextPtr v8Val [] (Proxy :: Proxy 'True)
{-
    pure $ Right $ \a -> unsafePerformIO $ do
      jarg <- coerce $ getJSArg jsArg isolate a :: IO (Ptr ())
      resPtr <- withMVar globalContexts $ \gContextPtr -> [C.block| void* {
        v8::Isolate* isolate = static_cast<v8::Isolate*>($(void* isolate'));
        v8::EscapableHandleScope handle_scope(isolate);

        // v8::Global<v8::Context>* gContext = static_cast<v8::Global<v8::Context>*>($(void* gContextPtr));
        v8::Global<v8::Context>* gContext = static_cast<v8::Global<v8::Context>*>($(void* funcContextPtr));
        v8::Local<v8::Context> context = v8::Local<v8::Context>::New(isolate, *gContext);
        if (context.IsEmpty()) {
         fprintf(stderr, "Error creating context \n");
        }

        v8::Context::Scope context_scope(context);


        v8::Value* funcVal = static_cast<v8::Value*>($(void* v8Val'));
        v8::Function* func = v8::Function::Cast(funcVal);

        v8::Global<v8::Value>* gArgs = static_cast<v8::Global<v8::Value>*>($(void* jarg));
        v8::Local<v8::Value> jargs = v8::Local<v8::Value>::New(isolate, *gArgs);

        v8::Local<v8::Value> args[] = {v8::Number::New(isolate, 1323) };
        v8::Local<v8::Value> args1[] = {jargs};
        auto globalRecv = v8::Isolate::GetCurrent()->GetCurrentContext()->Global();
        auto funcResM = func->Call(context, globalRecv, 1, args1);

        // Clean up globals
        delete gContext;
        delete gArgs;
        if(!funcResM.IsEmpty()) {
         v8::Local<v8::Value> funcRes = funcResM.ToLocalChecked();
         return *(handle_scope.Escape(funcRes));
        } else { return NULL; }
       }|]
      print resPtr
      Right res <- runJSValue jsValue isolate (coerce resPtr)
      pure res
-}
type family Arity (t :: Type) :: Nat where
  Arity (a -> b) = 1 + Arity b
  Arity _        = 0

type family IsFun (t :: Type) :: Bool where
  IsFun (a -> b) = 'True
  IsFun _        = 'False

{-
type family IsPure (t :: Type) :: Bool where
  IsPure (_ -> r) = IsPure r
  IsPure (IO _)   = 'True
  IsPure _        = 'False
-}
  
class MkJSFun t (isFun :: Bool) where
  mkJSFun :: Ptr Isolate -> Ptr (Global Context) -> Ptr V8Value -> [Ptr (Global V8Value)] -> Proxy isFun -> t

instance ( MkJSFun b (IsFun b)
         , InjectJS a
         ) => MkJSFun (a -> b) 'True where
  mkJSFun isolate gContext fnVal args _ = \a -> unsafePerformIO $ do
    jarg <- getJSArg jsArg isolate a
    pure $ mkJSFun isolate gContext fnVal (jarg:args) (Proxy :: Proxy (IsFun b))

instance InterpretJS t => MkJSFun t 'False where
  mkJSFun isolate gContext fnVal args _ = unsafePerformIO $ do
    let
      argVec = SV.fromList $ reverse args
      argCount = fromIntegral $ SV.length argVec
    when (argCount >= 256) $ error "Panic: Reached maximum number argument that can be passed"
    resPtr <- SV.unsafeWith argVec $ \argVecPtr -> [C.block| ValuePtr {
        v8::Isolate* isolate = static_cast<v8::Isolate*>($(IsolatePtr isolate));
        v8::EscapableHandleScope handle_scope(isolate);


        v8::Global<v8::Context>* gContext = static_cast<v8::Global<v8::Context>*>($(GlobalContextPtr gContext));
        v8::Local<v8::Context> context = v8::Local<v8::Context>::New(isolate, *gContext);
        if (context.IsEmpty()) {
         fprintf(stderr, "Error creating context \n");
        }

        v8::Context::Scope context_scope(context);

        v8::Value* funcVal = static_cast<v8::Value*>($(ValuePtr fnVal));
        v8::Function* func = v8::Function::Cast(funcVal);


        GlobalValuePtr* globalArgVec = $(GlobalValuePtr* argVecPtr);
        v8::Local<v8::Value> args[256];

        for(int64_t i=0; i < $(int64_t argCount); i++){
          v8::Global<v8::Value>* gArg = static_cast<v8::Global<v8::Value>*>(globalArgVec[i]);
          args[i] = v8::Local<v8::Value>::New(isolate, *gArg);
        }


        auto globalRecv = v8::Isolate::GetCurrent()->GetCurrentContext()->Global();
        auto funcResM = func->Call(context, globalRecv, $(int64_t argCount), args);
        // Clean up globals
        delete gContext;
        for(int i=0; i < $(int64_t argCount); i++){
          v8::Global<v8::Value>* gArg = static_cast<v8::Global<v8::Value>*>(globalArgVec[i]);
          delete gArg;
        }

        if(!funcResM.IsEmpty()) {
         v8::Local<v8::Value> funcRes = funcResM.ToLocalChecked();
         return *(handle_scope.Escape(funcRes));
        } else { return NULL; }
       }|]
    print resPtr
    Right res <- runJSValue jsValue isolate resPtr
    pure res
  

newtype JSArg a = JSArg {getJSArg :: Ptr Isolate -> a -> IO (Ptr (Global V8Value))}

data InjectType
  = InjectByRef
  | InjectByValue
  
class InjectJS a where
  type JSTypeName a :: Symbol
  type JSTypeName a = ""
  
  type JSConstructorName a :: [(Symbol, Symbol)]
  type JSConstructorName a = '[]
  
  type JSInjectType a :: InjectType
  type JSInjectType a = 'InjectByRef
  
  jsArg :: JSArg a

instance InjectJS Int32 where
  jsArg = JSArg $ \isolate i -> do
    [UnsafeC.block|GlobalValuePtr {
      v8::Isolate* isolate = static_cast<v8::Isolate*>($(IsolatePtr isolate));
      auto n = v8::Number::New(isolate, $(int32_t i));
      v8::Global<v8::Value>* gVal = new v8::Global<v8::Value>(isolate, n);
      return gVal;
      }|]

instance InjectJS Word32 where
  jsArg = JSArg $ \isolate i -> do
    [UnsafeC.block|GlobalValuePtr {
      v8::Isolate* isolate = static_cast<v8::Isolate*>($(IsolatePtr isolate));
      auto n = v8::Number::New(isolate, $(uint32_t i));
      v8::Global<v8::Value>* gVal = new v8::Global<v8::Value>(isolate, n);
      return gVal;
      }|]

instance (InterpretJS a, InterpretJS b) => InjectJS (a, b) where
  jsArg = undefined
    

injectByRef :: Ptr Isolate -> a -> IO (Ptr ())
injectByRef isolate a = do
  sptr <- newStablePtr a
  let
    freeSP = freeStablePtr sptr
    hsPtr = castStablePtrToPtr sptr
  [UnsafeC.block|void* {
    v8::Isolate* isolate = static_cast<v8::Isolate*>($(IsolatePtr isolate));
    void* hsPtr = $(void* hsPtr);
    
    return NULL;
  }|]
  undefined

class HsTypeTemplate (t :: Type) where
  hsTypeTemplate :: Ptr Isolate -> Proxy t -> IO (Ptr (Global ObjectTemplate))

instance (SetAccessor a, SetAccessor b) => HsTypeTemplate (a, b) where
  hsTypeTemplate isolate _ = do
    gObjTpl <- [UnsafeC.block|GlobalObjectTemplatePtr {
      v8::Isolate* isolate = static_cast<v8::Isolate*>($(IsolatePtr isolate));

      v8::Local<v8::ObjectTemplate> result = v8::ObjectTemplate::New(isolate);
      result->SetInternalFieldCount(2);

      v8::Global<v8::ObjectTemplate>* gObjTpl = new v8::Global<v8::ObjectTemplate>(isolate, result);
    
      return gObjTpl;
    }|]
    setAccessor isolate "_1" 0 (Proxy :: Proxy a) gObjTpl
    setAccessor isolate "_2" 0 (Proxy :: Proxy b) gObjTpl
    pure gObjTpl
    

class SetAccessor t where
  setAccessor :: Ptr Isolate -> String -> Word32 -> Proxy t -> (Ptr (Global ObjectTemplate) -> IO ())

instance SetAccessor Int32 where
  setAccessor isolate fldName pos _ gObjTpl = withCString fldName $ \fldNameCStr -> do 
    let
      pos' = coerce pos
    [UnsafeC.block|void {
       v8::Isolate* isolate = static_cast<v8::Isolate*>($(IsolatePtr isolate));
       auto gObjTpl = static_cast<v8::Global<v8::ObjectTemplate>*>($(GlobalObjectTemplatePtr gObjTpl));
       v8::Local<v8::ObjectTemplate> objTpl = v8::Local<v8::ObjectTemplate>::New(isolate, *gObjTpl);
       int pos = $(uint32_t pos');
       auto fldName = v8::String::NewFromUtf8(isolate, $(char* fldNameCStr), v8::NewStringType::kInternalized)
              .ToLocalChecked();
       objTpl->SetAccessor
         ( fldName
         , [](v8::Local<v8::String> name, const v8::PropertyCallbackInfo<v8::Value>& info) {
             uint32_t pos = v8::Uint32::Cast(*info.Data())->Value();
             auto holderObj = info.Holder();
             v8::Local<v8::External> field = v8::Local<v8::External>::Cast(holderObj->GetInternalField(pos));
             int32_t* val = static_cast<int32_t*>(field->Value());
             info.GetReturnValue().Set(*val);
           }
         , [](v8::Local<v8::String> name, v8::Local<v8::Value> value, const v8::PropertyCallbackInfo<void>& info) {
             uint32_t pos = v8::Uint32::Cast(*info.Data())->Value();
             auto holderObj = info.Holder();
             v8::Local<v8::External> field = v8::Local<v8::External>::Cast(holderObj->GetInternalField(pos));
             int32_t* val = static_cast<int32_t*>(field->Value());
             *val = 10;
           }
         , v8::Uint32::New(isolate, pos));
     }|]
  
class GHsTypeTemplate (f :: Type -> Type) where
  gHsTypeTemplate :: Ptr Isolate -> f a -> IO (Ptr ObjectTemplate)

instance GHsTypeTemplate f => GHsTypeTemplate (M1 c i f) where
  gHsTypeTemplate isolate (M1 a) = gHsTypeTemplate isolate a

instance (GHsTypeTemplate f, GHsTypeTemplate g) => GHsTypeTemplate (f :*: g) where
  gHsTypeTemplate isolate (f :*: g) = gHsTypeTemplate isolate f

instance () => GHsTypeTemplate (K1 i a) where
  gHsTypeTemplate isolate (K1 a) = undefined

runJS :: (InterpretJS t) => Text -> IO t
runJS jsText = do
  withIsolate $ \iso -> withContext (do
    v8Value <- case parseJSExpr jsText of
      JsInlineExpr inlineExpr -> execInlineJSExpr inlineExpr iso
      JsImportedExpr expr modSpec -> execImportedJSExpr expr modSpec
    res <- runJSValue jsValue iso v8Value
    case res of
      Left e -> error "cast failure"
      Right r -> pure r
                                    ) iso

runJS' :: (InterpretJS t) => Ptr Isolate -> Text -> IO t
runJS' iso jsText = do
    v8Value <- case parseJSExpr jsText of
      JsInlineExpr inlineExpr -> execInlineJSExpr inlineExpr iso
      JsImportedExpr expr modSpec -> execImportedJSExpr expr modSpec
    res <- runJSValue jsValue iso v8Value
    case res of
      Left e -> error "cast failure"
      Right r -> pure r
                                     

data JsExprType
  = JsInlineExpr Text
  | JsImportedExpr Text Text
  deriving (Show, Eq)

execInlineJSExpr :: Text -> Ptr Isolate -> IO (Ptr V8Value)
execInlineJSExpr jsExpr isolate  = do
  scriptPtr <- compileInlineJS jsExpr isolate
  runInlineJS scriptPtr isolate

execImportedJSExpr :: Text -> Text -> IO (Ptr V8Value)
execImportedJSExpr jsExpr modSpec = do
  pure undefined  
  

--newtype Isolate = Isolate { getIsolate :: Ptr () }
--  deriving (Show, Eq)


{-
data V8Platform = V8Platform
  { isInitialized :: Bool
  , isolates :: [Ptr ()]
  }
-}

{-
v8PlatformRef :: MVar V8Platform
v8PlatformRef = unsafePerformIO $ newMVar $ V8Platform
  { isInitialized = False
  , isolates = []
  }
{-# NOINLINE v8PlatformRef #-}


initV8 :: IO V8Platform
initV8 = do
  modifyMVar v8PlatformRef $ \v8Platform -> case isInitialized v8Platform of
    True -> pure (v8Platform, v8Platform)
    False -> do
      progName <- getProgName
      isolate <- (withCString progName $ \progNamePtr -> do
        [C.block|void* {
            v8::V8::InitializeICUDefaultLocation($(char* progNamePtr));
            v8::V8::InitializeExternalStartupData($(char* progNamePtr));
            std::unique_ptr<v8::Platform> platform = v8::platform::NewDefaultPlatform();
    
            v8::V8::InitializePlatform(platform.get());
            v8::V8::Initialize();

            // Create a new Isolate and make it the current one.
            v8::Isolate::CreateParams create_params;
            create_params.array_buffer_allocator =
              v8::ArrayBuffer::Allocator::NewDefaultAllocator();
            v8::Isolate* isolate = v8::Isolate::New(create_params);
  
            v8::Isolate::Scope isolate_scope(isolate);
            v8::HandleScope handle_scope(isolate);
            v8::Platform* i = platform.get();
            printf("Address of i is %p\n", (void *)i);
            printf("ISOLATE: %f\n", i->MonotonicallyIncreasingTime());
            return i;          
          }|])
      print isolate
      let v8p = v8Platform {isInitialized = True, isolates = [isolate]}
      pure (v8p, v8p)

getIsolate' :: V8Platform -> Ptr ()
getIsolate' v8 = case isolates v8 of
  []       -> error "V8 Platform is not initialized"
  (iso: _) -> iso

-}

withV8 :: (Ptr V8Platform -> IO a) -> IO a
withV8 v8App = do
  resVar <- newIORef Nothing
  v8AppCB <- $(C.mkFunPtr [t| Ptr V8Platform -> IO ()|]) (\vp -> v8App vp >>= \res -> writeIORef resVar (Just res))
  progName <- getProgName
  withCString progName $ \progNamePtr -> do
   [C.block|void {
      v8::V8::InitializeICUDefaultLocation($(char* progNamePtr));
      v8::V8::InitializeExternalStartupData($(char* progNamePtr));
      std::unique_ptr<v8::Platform> platform = v8::platform::NewDefaultPlatform();
    
      v8::V8::InitializePlatform(platform.get());
      v8::V8::Initialize();

      void (*v8App)(PlatformPtr) = $(void (*v8AppCB)(PlatformPtr));
      v8App(platform.get());

      v8::V8::Dispose();
      v8::V8::ShutdownPlatform();

    }|]
  resM <- readIORef resVar
  maybe (error "Panic: @withV8: Unable retrieve result") pure resM

withV8_ :: IO a -> IO a
withV8_ v8App = withV8 (const v8App)

withIsolate :: (Ptr Isolate -> IO a) -> IO a
withIsolate onIsolate = do
  resVar <- newIORef Nothing
  onIsolateCB <- $(C.mkFunPtr [t| Ptr Isolate -> IO ()|]) (\iso -> onIsolate iso >>= \res -> writeIORef resVar (Just res))
  initGlobalContexts <- $(C.mkFunPtr [t|Ptr () -> IO ()|]) (\gCtxPtr ->
    do
      putMVar globalContexts gCtxPtr
      )
  [C.block|void {
      // Create a new Isolate and make it the current one.
      v8::Isolate::CreateParams create_params;
      create_params.array_buffer_allocator =
        v8::ArrayBuffer::Allocator::NewDefaultAllocator();
      v8::Isolate* isolate = v8::Isolate::New(create_params);
      {
        v8::Isolate::Scope isolate_scope(isolate);
        v8::HandleScope handle_scope(isolate);

        v8::Global<v8::Context> gContext;
        void (*initGlobalContexts)(void*) = $(void (*initGlobalContexts)(void*));
        initGlobalContexts(&gContext);

        void (*onIsolate)(IsolatePtr) = $(void (*onIsolateCB)(IsolatePtr));
        onIsolate(isolate);
      }
      isolate->Dispose();
      delete create_params.array_buffer_allocator;
  }|]
  resM <- readIORef resVar
  maybe (error "Panic: @withIsolate: Unable retrieve result") pure resM

globalContexts :: MVar (Ptr ())
globalContexts = unsafePerformIO $ newEmptyMVar
{-# NOINLINE globalContexts #-}

{-
preludeTemplates :: MVar (Ptr (Global ObjectTemplate))
preludeTemplates = unsafePerformIO $ newEmptyMVar
{-# NOINLINE preludeTemplates #-}
-}

withContext :: IO a -> Ptr Isolate -> IO a
withContext v8Context isolate = do
  resVar <- newIORef Nothing
  v8ContextCB <- $(C.mkFunPtr [t|IO ()|]) (v8Context >>= \res -> writeIORef resVar (Just res))

  [C.block|void {
    v8::Isolate* isolate = static_cast<v8::Isolate*>($(IsolatePtr isolate));
    v8::Local<v8::Context> context = v8::Context::New(isolate);

    v8::Context::Scope context_scope(context);
    {
      void (*v8Context)() = $(void (*v8ContextCB)());
      v8Context();
    }
    
  }|]
  resM <- readIORef resVar
  maybe (error "Panic: @withContext: Unable retrieve result") pure resM

newtype TypeTemplate = TypeTemplate {getTypeTemplateMap :: Map TypeRep (Ptr ObjectTemplate)}

withContext' :: (IO a) -> Ptr Isolate -> IO a
withContext' v8Context isolate = do
  resVar <- newIORef Nothing
  v8ContextCB <- $(C.mkFunPtr [t|IO ()|]) (v8Context >>= \res -> writeIORef resVar (Just res))
  tupTpl <- hsTypeTemplate isolate (Proxy :: Proxy (Int32, Int32))
--  glo <- [UnsafeC.exp|GlobalObjectTemplatePtr { new v8::Global<v8::ObjectTemplate>() }|]
  preludeTemplates <- newEmptyMVar
  withMVar globalContexts $ \gContextPtr -> [C.block|void {
    v8::Isolate* isolate = static_cast<v8::Isolate*>($(IsolatePtr isolate));

    v8::Local<v8::ObjectTemplate> global = v8::ObjectTemplate::New(isolate);
    //v8::Local<v8::Value> g = global;
    global->Set(v8::String::NewFromUtf8(isolate, "log"),
            v8::FunctionTemplate::New(isolate, [](const v8::FunctionCallbackInfo<v8::Value>& args){
        if (args.Length() < 1) return;
        v8::Isolate* isolate = args.GetIsolate();
        v8::HandleScope scope(isolate);
        v8::Local<v8::Value> arg = args[0];
        v8::String::Utf8Value value(isolate, arg);

        uint64_t d = v8::BigInt::Cast(*args.Data())->Uint64Value();
        
        printf("hellooooooooooooooooooooooo %s %lu\n", *value, d);

        std::map<std::pair<uint64_t, uint64_t>, int> testM;

//        v8::Global<v8::ObjectTemplate>* gObjTpl = static_cast<v8::Global<v8::ObjectTemplate>*>($(GlobalObjectTemplatePtr tupTpl));
//        v8::Local<v8::ObjectTemplate> objTpl = v8::ObjectTemplate::New(isolate, *gObjTpl);
//        v8::Local<v8::Object> result = objTpl->NewInstance(isolate->GetCurrentContext()).ToLocalChecked();
//        result->SetInternalField(0, v8::Number::New(isolate, 1323));
//        result->SetInternalField(1, v8::Number::New(isolate, 2323));

        args.GetReturnValue().Set(v8::Uint32::New(isolate, 1323));

     }, v8::BigInt::NewFromUnsigned(isolate, 1323)));

    v8::Local<v8::Context> context = v8::Context::New(isolate, NULL, global);
    v8::Global<v8::Context>* gContext = static_cast<v8::Global<v8::Context>*>($(void* gContextPtr));
    gContext->Reset(isolate, context);

    v8::Context::Scope context_scope(context);
    {
      void (*v8Context)() = $(void (*v8ContextCB)());
      v8Context();
    }
    
  }|]
  resM <- readIORef resVar
  maybe (error "Panic: @withContext: Unable retrieve result") pure resM


compileInlineJS :: Text -> Ptr Isolate -> IO (Ptr Script)
compileInlineJS jsText isolate = do
  withCString (T.unpack jsText) $ \jsStrPtr -> do
    [C.block|ScriptPtr {
        v8::Isolate* isolate = static_cast<v8::Isolate*>($(IsolatePtr isolate));
        v8::EscapableHandleScope handle_scope(isolate);
        v8::Local<v8::Context> context(isolate->GetCurrentContext());
  
        // Create a string containing the JavaScript source code.
        v8::Local<v8::String> source =
        v8::String::NewFromUtf8(isolate, $(const char* jsStrPtr),
                                v8::NewStringType::kNormal)
                    .ToLocalChecked();
        printf("Before compilation\n");
        v8::TryCatch try_catch(isolate);
        // Compile the source code.
        v8::Local<v8::Script> compiledScript;
        if(!v8::Script::Compile(context, source).ToLocal (&compiledScript)) {
          v8::String::Utf8Value error(isolate, try_catch.Exception());
          printf("JS Compile Err: %s\n", *error);
          return NULL;                                                                  
        }
        printf("After compilationnnnnnn\n");

        return *(handle_scope.Escape(compiledScript));
  
    }|]

runInlineJS :: Ptr Script -> Ptr Isolate -> IO (Ptr V8Value)
runInlineJS script isolate = do
  [C.block|ValuePtr {
    v8::Isolate* isolate = static_cast<v8::Isolate*>($(IsolatePtr isolate));
    v8::Script* compiledScript = static_cast<v8::Script*>($(ScriptPtr script));
    v8::EscapableHandleScope handle_scope(isolate);
    v8::Local<v8::Context> context(isolate->GetCurrentContext());
    v8::TryCatch try_catch(isolate);

    // Run the script to get the result.
    v8::Local<v8::Value> result;
    if(!compiledScript->Run(context).ToLocal(&result)){
      v8::String::Utf8Value error(isolate, try_catch.Exception());
      printf("JS RT Err: %s\n", *error);
      return NULL;
    }

    return *(handle_scope.Escape(result));
  }|]
  
execs :: Ptr Isolate -> IO ()
execs isolate = do
  [C.block|void {
      v8::Isolate* isolate = static_cast<v8::Isolate*>($(IsolatePtr isolate));
            v8::Local<v8::Context> context(isolate->GetCurrentContext());
                  // Create a string containing the JavaScript source code.
            v8::Local<v8::String> source =
                v8::String::NewFromUtf8(isolate, "1+1",
                                        v8::NewStringType::kNormal)
                    .ToLocalChecked();
            printf("Before compilation\n");
            v8::TryCatch try_catch(isolate);
            // Compile the source code.
            v8::Local<v8::Script> compiledScript;
            if(!v8::Script::Compile(context, source).ToLocal (&compiledScript)) {
              v8::String::Utf8Value error(isolate, try_catch.Exception());
              printf("JS Compile Err: %s\n", *error);
              return;                                                                  
            }
            printf("After compilation\n");

            // Run the script to get the result.
            v8::Local<v8::Value> result;
            if(!compiledScript->Run(context).ToLocal(&result)){
              v8::String::Utf8Value error(isolate, try_catch.Exception());
              printf("JS RT Err: %s\n", *error);
              return;
            }
            // Convert the result to an UTF8 string and print it.
            printf("After Run\n");
            v8::String::Utf8Value utf8(isolate, result);
            std::cout << result->IsNumber() << std::endl;
  }|]

parseJSExpr :: Text -> JsExprType
parseJSExpr js = either id id $ do
  case T.breakOn "from" js of
    (_,"") -> Left $ JsInlineExpr js
    (exprStr, modStr') ->
      let modStr = T.strip (T.drop 4 modStr')
      in case T.null modStr of
        True -> Left $ JsInlineExpr js
        False -> case (T.head modStr, T.last modStr) of
          ('\'', '\'') -> Right $ JsImportedExpr exprStr $ T.dropEnd 1 $ T.drop 1 modStr
          ('"', '"')   -> Right $ JsImportedExpr exprStr $ T.dropEnd 1 $ T.drop 1 modStr
          _            -> Left $ JsInlineExpr js
        
  
execJS' :: String -> IO ()
execJS' js = withV8_ $ do
  withCString js $ \jsStrPtr -> do
    [C.block|void {
      try {

        // Create a new Isolate and make it the current one.
        v8::Isolate::CreateParams create_params;
        create_params.array_buffer_allocator =
         v8::ArrayBuffer::Allocator::NewDefaultAllocator();
        v8::Isolate* isolate = v8::Isolate::New(create_params);
  
        printf ("After v8::Isolate::New\n");
        {
          v8::Isolate::Scope isolate_scope(isolate);
          // Create a stack-allocated handle scope.
          v8::HandleScope handle_scope(isolate);


          // Globals
          // v8::Local<v8::ObjectTemplate> global = v8::ObjectTemplate::New(isolate);
          //global->Set(
          //  v8::String::NewFromUtf8(isolate, "version", v8::NewStringType::kNormal).ToLocalChecked(),
          //  v8::FunctionTemplate::New(isolate, Version));

          printf ("Before v8::Context::New\n");
          // Create a new context.
          v8::Local<v8::Context> context = v8::Context::New(isolate);//, NULL, global);

          printf ("After v8::Context::New\n");

          // Enter the context for compiling and running the hello world script.
          v8::Context::Scope context_scope(context);
          {
            // Create a string containing the JavaScript source code.
            v8::Local<v8::String> source =
                v8::String::NewFromUtf8(isolate, $(const char* jsStrPtr),
                                        v8::NewStringType::kNormal)
                    .ToLocalChecked();
            printf("Before compilation\n");
            v8::TryCatch try_catch(isolate);
            // Compile the source code.
            v8::Local<v8::Script> compiledScript;
            if(!v8::Script::Compile(context, source).ToLocal (&compiledScript)) {
              v8::String::Utf8Value error(isolate, try_catch.Exception());
              printf("JS Compile Err: %s\n", *error);
              return;                                                                  
            }
            printf("After compilation\n");

            // Run the script to get the result.
            v8::Local<v8::Value> result;
            if(!compiledScript->Run(context).ToLocal(&result)){
              v8::String::Utf8Value error(isolate, try_catch.Exception());
              printf("JS RT Err: %s\n", *error);
              return;
            }
            // Convert the result to an UTF8 string and print it.
            printf("After Run\n");
            v8::String::Utf8Value utf8(isolate, result);
            std::cout << result->IsBigInt() << std::endl;
            if (result->IsFunction()){
              v8::Local<v8::Function> func = v8::Local<v8::Function>::Cast(result);
              v8::Local<v8::Value> args[] = { v8::Number::New(isolate, 1323) };
              printf("Before JS Fun Call\n");
              auto globalRecv = v8::Isolate::GetCurrent()->GetCurrentContext()->Global();
              auto funcResM = func->Call(context, globalRecv, 1, args);
              if(!funcResM.IsEmpty()) {
                auto funcRes = funcResM.ToLocalChecked()->ToBigInt(context).ToLocalChecked()->Int64Value();
                std::cout << "I64 Fn Res: " << funcRes << std::endl;
              }
              
              printf("After JS Fun Call\n");
            }
            auto i64M = result->IntegerValue(context);
            auto bigiM = result->ToBigInt(context);
            auto i64 = (bigiM.IsEmpty()) ? 0 : bigiM.ToLocalChecked()->Int64Value();
            std::cout << "I64: " << i64 << std::endl;
            printf("%s\n", *utf8);
          }
        }
        printf("Hello from c++\n");
      } catch (const std::runtime_error &c) {
        printf("Hello from c++ execption \n");
      }
    }|]

C.verbatim " \ 
\ void GetPointX(v8::Local<v8::String> property, \
\               const v8::PropertyCallbackInfo<v8::Value>& info) { \
\  v8::Local<v8::Object> self = info.Holder(); \
\  v8::Local<v8::External> wrap = v8::Local<v8::External>::Cast(self->GetInternalField(0)); \
\  void* ptr = wrap->Value(); \
\  int value = 0; \
\  info.GetReturnValue().Set(value); \
\ }"


testGhcBug11829 :: IO Int
testGhcBug11829 = do
  fromIntegral <$> [C.block|int {
      try {
          throw std::runtime_error("THIS FAILS!");
      } catch(const std::runtime_error &c) {
        return 0;
      }
  }|]


jsq :: QuasiQuoter
jsq = QuasiQuoter
  { quoteExp = \s -> [|s|]
  , quotePat = error "jsq does not support Pat"
  , quoteType = error "jsq does not support Type"
  , quoteDec = error "jsq does not support Dec"
  }


