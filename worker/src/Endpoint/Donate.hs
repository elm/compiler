{-# LANGUAGE OverloadedStrings #-}
module Endpoint.Donate
  ( Manager
  , endpoint
  , getManager
  )
  where


import qualified Control.Exception as E
import Control.Monad.Trans (liftIO)
import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Snap.Core
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http (tlsManagerSettings)
import qualified Network.HTTP.Types.Header as Http (Header, hAccept, hAcceptEncoding, hUserAgent)
import qualified Network.HTTP.Types.Method as Http (methodPost)
import qualified Network.HTTP.Client as Multi (RequestBody(RequestBodyLBS))
import qualified Network.HTTP.Client.MultipartFormData as Multi

import qualified Cors



-- ALLOWED ORIGINS


allowedOrigins :: [String]
allowedOrigins =
  [ "https://foundation.elm-lang.org"
  ]



-- GET MANAGER


newtype Manager =
  Manager Http.Manager


getManager :: IO Manager
getManager =
  Manager <$> Http.newManager Http.tlsManagerSettings



-- ENDPOINT


endpoint :: Manager -> Snap ()
endpoint (Manager manager) =
  Cors.allow POST allowedOrigins $
    do  amount <- requireParameter "amount" toAmount
        frequency <- requireParameter "frequency" toFrequency
        mabyeSession <- liftIO $ getStripeCheckoutSessionID manager amount
        case mabyeSession of
          Just (StripeCheckoutSession id) ->
            do  modifyResponse $ setContentType "text/plain; charset=utf-8"
                writeText id

          Nothing ->
            do  writeBuilder $ "Problem creating Stripe session ID for checkout."
                finishWith
                  . setResponseStatus 500 "Internal Server Error"
                  . setContentType "text/plain; charset=utf-8"
                  =<< getResponse


data Frequency
  = OneTime
  | Monthly


toFrequency :: BS.ByteString -> Maybe Frequency
toFrequency bytes =
  case bytes of
    "onetime" -> Just OneTime
    "monthly" -> Just Monthly
    _         -> Nothing


toAmount :: BS.ByteString -> Maybe Int
toAmount bytes =
  if BS.all (\w -> 0x30 <= w && w <= 0x39) bytes
    && not (BS.isPrefixOf "0" bytes)
  then Just (BS.foldr (\w n -> 10 * n + fromIntegral (w - 0x30)) 0 bytes)
  else Nothing



-- GET STRIPE CHECKOUT SESSION ID


newtype StripeCheckoutSession =
  StripeCheckoutSession { _id :: T.Text }


getStripeCheckoutSessionID :: Http.Manager -> Int -> IO (Maybe StripeCheckoutSession)
getStripeCheckoutSessionID manager amount =
  E.handle handleSomeException $
  do  req0 <- Http.parseRequest "https://api.stripe.com/v1/checkout/sessions"
      req1 <- Multi.formDataBody (toOneTimeParts amount) $ req0 { Http.method = Http.methodPost }
      Http.withResponse req1 manager $ \response ->
        do  chunks <- Http.brConsume (Http.responseBody response)
            return $ Json.decode $ LBS.fromChunks chunks


toOneTimeParts :: Int -> [Multi.Part]
toOneTimeParts amount =
  [ Multi.partBS "payment_method_types[]" "card"
  , Multi.partBS "line_items[][name]" "One-time donation"
  , Multi.partBS "line_items[][description]" "One-time donation to Elm Software Foundation"
  , Multi.partBS "line_items[][images][]" "https://foundation.elm-lang.org/donation.png"
  , Multi.partBS "line_items[][amount]" (BSC.pack (show amount))
  , Multi.partBS "line_items[][currency]" "usd"
  , Multi.partBS "line_items[][quantity]" "1"
  , Multi.partBS "success_url" "https://foundation.elm-lang.org/thank_you?session_id={CHECKOUT_SESSION_ID}"
  , Multi.partBS "cancel_url" "https://foundation.elm-lang.org/donate"
  ]


handleSomeException :: E.SomeException -> IO (Maybe a)
handleSomeException exception =
  return Nothing


instance Json.FromJSON StripeCheckoutSession where
  parseJSON =
    Json.withObject "StripeCheckoutSessionResponse" $ \obj ->
      StripeCheckoutSession <$> obj .: "id"



-- REQUIRE PARAMETER


requireParameter :: BS.ByteString -> (BS.ByteString -> Maybe a) -> Snap a
requireParameter name toValue =
  do  params <- getsRequest (rqParam name)
      case params of
        Just [bytes] ->
          case toValue bytes of
            Just value -> return value
            Nothing    -> bailForMissingParam name

        _ ->
          bailForMissingParam name


bailForMissingParam :: BS.ByteString -> Snap a
bailForMissingParam name =
  do  writeBuilder $ "Missing parameter '" <> B.byteString name <> "' in requset."
      finishWith
        . setResponseStatus 400 "Bad Request"
        . setContentType "text/plain; charset=utf-8"
        =<< getResponse
