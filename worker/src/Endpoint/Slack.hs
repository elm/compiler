{-# LANGUAGE OverloadedStrings #-}
module Endpoint.Slack
  ( endpoint
  )
  where


import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client
import Snap.Core

import qualified Cors

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map



-- Send invitations to the Elm Slack to whoever asks.
--
-- NOTE: The API to invite users is not officially documented, but the people
-- here looked in the Network tab of Developer Tools to figure it out:
--
--   https://levels.io/slack-typeform-auto-invite-sign-ups/
--   https://github.com/outsideris/slack-invite-automation
--



-- ALLOWED ORIGINS


allowedOrigins :: [String]
allowedOrigins =
  [ "https://elm-lang.org"
  ]



-- ENDPOINT


endpoint :: String -> Manager -> Snap ()
endpoint token manager =
  Cors.allow POST allowedOrigins $
    do  req <- getRequest
        case Map.findWithDefault [] "email" (rqQueryParams req) of
          [email] ->
            do  response <- liftIO $ httpLbs (request email) manager
                modifyResponse $ setContentType "application/json"
                writeLBS (responseBody response)

          _ ->
            do  modifyResponse $ setResponseStatus 400 "Bad Request"
                modifyResponse $ setContentType "text/html; charset=utf-8"
                writeBS "expecting query parameter like ?email=you@example.com"
  where
    slack_token =
      BSC.pack token

    request email =
      urlEncodedBody
        [ ("email", email)
        , ("token", slack_token)
        , ("set_active","true")
        ]
        (parseRequest_ "https://elmlang.slack.com/api/users.admin.invite")
