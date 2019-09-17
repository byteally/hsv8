module Language.JavaScript.Internal where


import qualified Data.Map as Map
import qualified Language.C.Inline.Cpp as C
import qualified Language.C.Inline as C
import qualified Language.C.Types as C
import Language.C.Inline.Context (ctxTypesTable)
import Language.Haskell.TH
import Data.Monoid


v8Ctx :: [(C.CIdentifier, TypeQ)] -> C.Context
v8Ctx types = C.cppCtx <> v8Ctx'
  where v8Ctx' = mempty { ctxTypesTable = v8TypesTable types}

v8TypesTable :: [(C.CIdentifier, TypeQ)] -> Map.Map C.TypeSpecifier TypeQ
v8TypesTable types = Map.fromList (fmap (\(f,s) -> (C.TypeName f, s)) types)
