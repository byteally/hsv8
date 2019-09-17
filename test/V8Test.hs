{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module V8Test where

import Test.Tasty
import Test.Tasty.HUnit
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty.Hedgehog
import Language.JavaScript
import Data.Text (Text)
import qualified Data.Text as T
import Data.Int
import Data.Word
import Language.Haskell.TH.Quote
import qualified Data.Vector.Storable as SV


data HsTy = HsTy
  { strF :: Text
  , intF :: Int
  } deriving  (Show, Eq)

instance InjectJS HsTy where

instance InterpretJS HsTy where

instance InterpretJS Int where

instance InjectJS Int where

unit_foo :: IO ()
unit_foo = withV8_ $ do
  -- execJS' "const big_array = new BigInt64Array(1);big_array[0] = 9223372036854775807n;n => big_array[0] - n;"
  -- execJS' "n => {if (n!==1)return BigInt(n) + 1000n; else return 0n}"
{-  
  withIsolate $ \iso -> do
    withContext (pure ()) iso
    withContext (pure ()) iso
    withContext (execs iso) iso
    withContext (do
                    scriptPtr <- compileInlineJS "1" iso
                    print (scriptPtr)
                    val <- runInlineJS scriptPtr iso
                    print val
                    pure ()
                ) iso
  runJS @Int32 "12132 + 23" >>= print
  runJS @Int64 "12132433435n" >>= print 
  runJS @Word32 "12132 + 28" >>= print
  runJS @(Int32, Word32) "[1,12]" >>= print
  runJS @(SV.Vector Int32) "[1,12,23,4+1]" >>= print
-}
  withIsolate $ \iso -> do
    res <- withContext' (do
                    runJS' @Int64 iso "12132433435n" >>= print 
                    runJS' @Word32 iso "12132 + 28" >>= print
                    runJS' @(SV.Vector Int32) iso "[1,12,'a',4+1]" >>= print
                    --runJS' @(Int32, Word32) iso "[1,12]" >>= print
                    fn <- runJS' @(Int32 -> Word32 -> Int32 -> Int32) iso "(i, j, k) => {x = log('aad'); return i + j + k + x;}"    
                    pure $ (fn)
                ) iso
    print (res 10 11 12)
--  r <- runJS (jsValue @Int32) "(hsty => hsty.strF)"
--  testFn <- runJS (jsValue :: JSValue (HsTy -> HsTy)) "n => n + 3"
  pure ()

jsFn = undefined  

-- https://ghc.haskell.org/trac/ghc/ticket/11829
_unit_GhcBug11829 :: IO ()
_unit_GhcBug11829 = do
  res <- testGhcBug11829
  print res
  pure ()

testJSMod :: String
testJSMod = [jsq|
import * from './.'

export x = 10
|]             

