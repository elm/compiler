module Libraries (libraries) where

import qualified Data.Map as Map
import Text.JSON
import LoadLibraries (docs)

libraries :: Map.Map String (Map.Map String String)
libraries = case getLibs of
              Ok libs   -> libs
              Error err -> error err

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