{-# LANGUAGE OverloadedStrings #-}
module Endpoint.Donate
  ( Manager
  , endpoint
  , getManager
  )
  where


import Prelude hiding (id)
import qualified Control.Exception as E
import Control.Monad.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Trans (liftIO)
import Data.Aeson ((.:))
import qualified Data.Aeson as Json
import Data.Char (chr)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Snap.Core
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http (tlsManagerSettings)
import qualified Network.HTTP.Types.Header as Http (Header, hAccept, hAcceptEncoding, hUserAgent)
import qualified Network.HTTP.Types.Method as Http (methodPost)
import qualified Network.HTTP.Types.Status as Http (statusCode)

import qualified Cors



-- ALLOWED ORIGINS


allowedOrigins :: [String]
allowedOrigins =
  [ "https://foundation.elm-lang.org"
  ]



-- GET MANAGER
--
-- To talk to Stripe you need a header like this:
--
--   Authorization: Basic dXNlcm5hbWU6cGFzc3dvcmQ=
--                        ^^^^^^^^^^^^^^^^^^^^^^^^
-- Where the underlined part is the base64 encoded version of your Stripe
-- secret key. The secret key is given as an environment variable, and then
-- stored in the Manager as the "Authorization" value we will be using.
-- I figured this out based on the following links:
--
--   https://stripe.com/docs/payments/checkout/one-time
--   https://stackoverflow.com/a/35442984
--


data Manager =
  Manager
    { _manager :: Http.Manager
    , _authToken :: BS.ByteString
    }


getManager :: String -> IO Manager
getManager secret =
  do  manager <- Http.newManager Http.tlsManagerSettings
      return (Manager manager ("Basic " <> Base64.encode (BSC.pack secret)))


addAuthorization :: BS.ByteString -> Http.Request -> Http.Request
addAuthorization authToken req =
  req { Http.requestHeaders = ("Authorization", authToken) : Http.requestHeaders req }



-- ENDPOINT


endpoint :: Manager -> Snap ()
endpoint manager =
  Cors.allow POST allowedOrigins $
    do  cents <- requireParameter "cents" toCents
        frequency <- requireParameter "frequency" toFrequency
        result <- liftIO $ runExceptT $ getStripeCheckoutSessionID manager cents frequency
        case result of
          Right (StripeCheckoutSession id) ->
            do  modifyResponse $ setContentType "text/plain; charset=utf-8"
                writeText id

          Left _ ->
            do  writeBuilder $ "Problem creating Stripe session ID for checkout."
                finishWith
                  . setResponseStatus 500 "Internal Server Error"
                  . setContentType "text/plain; charset=utf-8"
                  =<< getResponse


data Frequency
  = OneTime
  | Monthly


toFrequency :: BS.ByteString -> Either B.Builder Frequency
toFrequency bytes =
  case bytes of
    "onetime" -> Right OneTime
    "monthly" -> Right Monthly
    _         -> Left "The only valid values are frequency=onetime and frequency=monthly."


toCents :: BS.ByteString -> Either B.Builder Int
toCents bytes =
  if BS.all (\w -> 0x30 <= w && w <= 0x39) bytes && not (BS.isPrefixOf "0" bytes) then
    let
      cents = BS.foldl (\n w -> 10 * n + fromIntegral (w - 0x30)) 0 bytes
    in
    if cents >= 500
    then Right cents
    else
      Left
        "Processing fees are (2.2% + $0.30) per transaction, so the minimum\n\
        \donation is $5 (cents=500) to limit the worst case fee to ~8%"
  else
    Left "Must be a value like cents=1000 for $10 or cents=2500 for $25."



-- GET STRIPE CHECKOUT SESSION ID


newtype StripeCheckoutSession =
  StripeCheckoutSession { _id :: T.Text }


instance Json.FromJSON StripeCheckoutSession where
  parseJSON =
    Json.withObject "StripeCheckoutSessionResponse" $ \obj ->
      StripeCheckoutSession <$> obj .: "id"


getStripeCheckoutSessionID :: Manager -> Int -> Frequency -> Http StripeCheckoutSession
getStripeCheckoutSessionID manager cents frequency =
  case frequency of
    OneTime -> setupOnetimeDonation manager cents
    Monthly -> setupMonthlyDonation manager cents



-- SET UP ONE-TIME DONATION


setupOnetimeDonation :: Manager -> Int -> Http StripeCheckoutSession
setupOnetimeDonation manager cents =
  post manager
    "https://api.stripe.com/v1/checkout/sessions"
    [ "payment_method_types[]" ==> "card"
    , "line_items[][name]"     ==> "One-time donation"
    , "line_items[][images][]" ==> "https://foundation.elm-lang.org/donation.png"
    , "line_items[][amount]"   ==> BSC.pack (show cents)
    , "line_items[][currency]" ==> "usd"
    , "line_items[][quantity]" ==> "1"
    , "submit_type"            ==> "donate"
    , "success_url"            ==> "https://foundation.elm-lang.org/thank_you?session_id={CHECKOUT_SESSION_ID}"
    , "cancel_url"             ==> "https://foundation.elm-lang.org/donate"
    ]



-- SET UP MONTHLY DONATION


setupMonthlyDonation :: Manager -> Int -> Http StripeCheckoutSession
setupMonthlyDonation manager cents =
  do  (Plan id) <- getMonthlyPlan manager cents
      post manager
        "https://api.stripe.com/v1/checkout/sessions"
        [ "payment_method_types[]" ==> "card"
        , "subscription_data[items][][plan]" ==> id
        , "success_url" ==> "https://foundation.elm-lang.org/thank_you?session_id={CHECKOUT_SESSION_ID}"
        , "cancel_url"  ==> "https://foundation.elm-lang.org/donate"
        ]



-- GET MONTHLY PLAN


getMonthlyPlan :: Manager -> Int -> Http Plan
getMonthlyPlan manager cents =
  do  result <- try $ get manager ("https://api.stripe.com/v1/plans/" ++ toPlanID cents)
      case result of
        Right plan -> return plan
        Left _     -> createMonthlyPlan manager cents


newtype Plan =
  Plan BS.ByteString


instance Json.FromJSON Plan where
  parseJSON =
    Json.withObject "StripeResponse" $ \obj ->
      Plan . T.encodeUtf8 <$> obj .: "id"


toPlanID :: Int -> String
toPlanID cents =
  "monthly_" ++ show cents



-- CREATE MONTHLY PLAN


createMonthlyPlan :: Manager -> Int -> Http Plan
createMonthlyPlan manager cents =
  post manager
    "https://api.stripe.com/v1/plans"
    [ "id"       ==> BSC.pack (toPlanID cents)
    , "amount"   ==> BSC.pack (show cents)
    , "currency" ==> "usd"
    , "interval" ==> "month"
    , "nickname" ==> toPlanNickname cents
    , "product"  ==> "prod_GtPzOm0QbweJIE"
    ]


toPlanNickname :: Int -> BS.ByteString
toPlanNickname cents =
  let
    (dollars, leftovers) = divMod cents 100
    (dimes,pennies) = divMod leftovers 10
  in
  BSC.pack $
    "Monthly $" ++ show dollars ++ [ '.', chr (0x30 + dimes), chr (0x30 + pennies) ]



-- HTTP


type Http a = ExceptT Error IO a


data Error
  = StripeError LBS.ByteString
  | UnexpectedJson LBS.ByteString
  | SomethingElse E.SomeException


try :: Http a -> ExceptT x IO (Either Error a)
try http =
  liftIO (runExceptT http)



-- HTTP GET


get :: (Json.FromJSON a) => Manager -> String -> Http a
get (Manager manager authToken) url =
  request manager $
    addAuthorization authToken <$> Http.parseRequest url



-- HTTP POST


post :: (Json.FromJSON a) => Manager -> String -> [(BS.ByteString, BS.ByteString)] -> Http a
post (Manager manager authToken) url parts =
  request manager $
    Http.urlEncodedBody parts . addAuthorization authToken <$> Http.parseRequest url


(==>) :: a -> b -> (a,b)
(==>) = (,)



-- HTTP REQUEST


request :: (Json.FromJSON a) => Http.Manager -> IO Http.Request -> Http a
request manager mkReq =
  ExceptT $ E.handle handleSomeException $
  do  req <- mkReq
      Http.withResponse req manager $ \response ->
        do  chunks <- Http.brConsume (Http.responseBody response)
            let code = Http.statusCode (Http.responseStatus response)
            let body = LBS.fromChunks chunks
            return $
              if 200 <= code && code < 300
              then
                case Json.decode body of
                  Just a  -> Right a
                  Nothing -> Left (UnexpectedJson body)
              else
                Left (StripeError body)


handleSomeException :: E.SomeException -> IO (Either Error a)
handleSomeException exception =
  return (Left (SomethingElse exception))



-- REQUIRE PARAMETER


requireParameter :: BS.ByteString -> (BS.ByteString -> Either B.Builder a) -> Snap a
requireParameter name toValue =
  do  params <- getsRequest (rqParam name)
      case params of
        Just [bytes] ->
          case toValue bytes of
            Right value ->
              return value

            Left message ->
              badParam $
                "Ran into invalid query parameter:\n\n    "
                <> B.byteString name <> "=" <> B.byteString bytes
                <> "\n\n" <> message

        _ ->
          badParam $
            "Missing parameter '" <> B.byteString name <> "' in requset."


badParam :: B.Builder -> Snap a
badParam message =
  do  writeBuilder message
      finishWith
        . setResponseStatus 400 "Bad Request"
        . setContentType "text/plain; charset=utf-8"
        =<< getResponse
