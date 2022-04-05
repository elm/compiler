{-# OPTIONS_GHC -Wall #-}
module Cors
  ( allow
  )
  where


import qualified Data.HashSet as HashSet
import Network.URI (parseURI)
import Snap.Core (Snap, Method, method)
import Snap.Util.CORS (CORSOptions(..), HashableMethod(..), OriginList(Origins), applyCORS, mkOriginSet)



-- ALLOW


allow :: Method -> [String] -> Snap () -> Snap ()
allow method_ origins snap =
  applyCORS (toOptions method_ origins) $ method method_ $
    snap



-- TO OPTIONS


toOptions :: (Monad m) => Method -> [String] -> CORSOptions m
toOptions method_ origins =
  let
    allowedOrigins = toOriginList origins
    allowedMethods = HashSet.singleton (HashableMethod method_)
  in
  CORSOptions
    { corsAllowOrigin = return allowedOrigins
    , corsAllowCredentials = return True
    , corsExposeHeaders = return HashSet.empty
    , corsAllowedMethods = return allowedMethods
    , corsAllowedHeaders = return
    }


toOriginList :: [String] -> OriginList
toOriginList origins =
  Origins $ mkOriginSet $
    case traverse parseURI origins of
      Just uris -> uris
      Nothing -> error "invalid entry given to toOriginList list"
