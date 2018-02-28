{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Develop.Compile
  ( toJavaScript
  )
  where


import qualified Data.ByteString.Char8 as BS



-- TO JAVASCRIPT


toJavaScript :: FilePath -> IO BS.ByteString
toJavaScript filePath =
  do  result <- error "TODO toJavaScript" filePath
      case result of
        Right (code, name) ->
          return $ BS.append code $ BS.pack $
            "var runElmProgram = Elm." ++ name ++ ".fullscreen;"

        Left errMsg ->
          return $ BS.concat $
            [ BS.pack $ "function runElmProgram() {\n\tElm.Errors.fullscreen("
            , error "TODO show compile issues" errMsg
            , ");\n}"
            ]

