module Libraries (libraries, addPrelude) where

import Ast
import qualified Data.Map as Map
import Data.List (inits)
import Text.JSON
import LoadLibraries (docs)

addPrelude :: Module -> Module
addPrelude (Module name exs ims stmts) = Module name exs (customIms ++ ims) stmts
    where customIms = concatMap addModule prelude

          addModule (n, method) = case lookup n ims of
                                    Nothing     -> [(n, method)]
                                    Just (As m) -> [(n, method)]
                                    Just _      -> []

prelude = text : map (\n -> (n, Hiding [])) modules
  where
    text = ("Graphics.Text", Hiding ["link", "color", "height"])
    modules = [ "Prelude", "Signal", "List", "Maybe", "Time"
              , "Graphics.Element", "Graphics.Color"
              , "Graphics.Collage", "Graphics.Geometry" ]

libraries :: Map.Map String (Map.Map String String)
libraries =
    case getLibs of
      Error err -> error err
      Ok libs   -> Map.unionWith Map.union libs nilAndTuples
          where nilAndTuples = Map.singleton "Prelude" (Map.fromList pairs)
                pairs =
                    [ ("Cons", "a -> [a] -> [a]")
                    , ("Nil", "[a]")
                    ] ++ map makeTuple (inits ['a'..'i'])
                
                makeTuple cs = 
                    let name = "Tuple" ++ show (length cs)
                    in  (name, concatMap (\c -> c : " -> ") cs ++
                               name ++ concatMap (\c -> [' ',c]) cs)

getLibs :: Result (Map.Map String (Map.Map String String))
getLibs = do
  obj <- decodeStrict docs :: Result (JSObject JSValue)
  modules <- valFromObj "modules" obj :: Result [JSObject JSValue]
  Map.fromList `fmap` mapM getValues modules


getName :: JSObject JSValue -> Result String
getName obj = valFromObj "name" obj

getType :: JSObject JSValue -> Result String
getType obj = valFromObj "type" obj

getValue :: JSObject JSValue -> Result (String,String)
getValue obj = do n <- getName obj
                  t <- getType obj
                  return (n,t)

getValues :: JSObject JSValue -> Result (String, Map.Map String String)
getValues obj = do
  name <- getName obj
  vs   <- valFromObj "values" obj
  vals <- mapM getValue vs
  return (name, Map.fromList vals)