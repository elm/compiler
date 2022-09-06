{-# LANGUAGE OverloadedStrings #-}
module Endpoint.Slack
  ( Token(..)
  , endpoint
  )
  where


-- Send invitations to the Elm Slack to whoever asks.
--
-- NOTE: The API to invite users is not officially documented, but the people
-- here looked in the Network tab of Developer Tools to figure it out:
--
--   https://levels.io/slack-typeform-auto-invite-sign-ups/
--   from https://github.com/outsideris/slack-invite-automation
--



-- ALLOWED ORIGINS


allowedOrigins :: [String]
allowedOrigins =
  [ "https://elm-lang.org"
  ]



-- ENDPOINT


newtype Token =
  Token BS.ByteString


endpoint :: Token -> Snap ()
endpoint (Token token) =
  Cors.allow POST allowedOrigins $
    do  req <- getRequest
        case Map.lookup "email" (rqQueryParams req) of
          [email] ->
            withResponse (request email) manager handler

          _ ->
          do  modifyResponse $ setResponseStatus 400 "Bad Request"
              modifyResponse $ setContentType "text/html; charset=utf-8"
              writeBS "expecting query parameter like ?email=you@example.com"
  where
    request email =
      urlEncodedBody
        [ ("email", email)
        , ("token", token)
        , ("set_active","true")
        ]
        (parseRequest_ "https://elmlang.slack.com/api/users.admin.invite")

    handler response =
      do  modifyResponse $ setContentType "application/json"
          loop (responseBody response)
      where
        loop body =
          do  chunk <- brRead body
              if BS.null chunk
                then return ()
                else
                  do  writeBS chunk
                      loop body

