{-# LANGUAGE GADTs #-}
module Terminal.Args.Internal
  ( Interface(..)
  , toName
  , Summary(..)
  , Flags(..)
  , Flag(..)
  , Parser(..)
  , Args(..)
  , CompleteArgs(..)
  , RequiredArgs(..)
  )
  where


import Text.PrettyPrint.ANSI.Leijen (Doc)



-- INTERFACE


data Interface where
  Interface
    :: String
    -> Summary
    -> String
    -> Doc
    -> Args args
    -> Flags flags
    -> (args -> flags -> IO ())
    -> Interface


toName :: Interface -> String
toName (Interface name _ _ _ _ _ _) =
  name



{-| The information that shows when you run the executable with no arguments.
If you say it is `Common`, you need to tell people what it does. Try to keep
it to two or three lines. If you say it is `Uncommon` you can rely on `Details`
for a more complete explanation.
-}
data Summary = Common String | Uncommon



-- FLAGS


data Flags a where
  FDone :: a -> Flags a
  FMore :: Flags (a -> b) -> Flag a -> Flags b


data Flag a where
  Flag :: String -> Parser a -> String -> Flag (Maybe a)
  OnOff :: String -> String -> Flag Bool



-- PARSERS


data Parser a =
  Parser
    { _singular :: String
    , _plural :: String
    , _parser :: String -> Maybe a
    , _suggest :: String -> IO [String]
    , _examples :: String -> IO [String]
    }



-- ARGS


newtype Args a =
  Args [CompleteArgs a]


data CompleteArgs args where
  Exactly  :: RequiredArgs args -> CompleteArgs args
  Multiple :: RequiredArgs ([a] -> args) -> Parser a -> CompleteArgs args
  Optional :: RequiredArgs (Maybe a -> args) -> Parser a -> CompleteArgs args


data RequiredArgs a where
  Done :: a -> RequiredArgs a
  Required :: RequiredArgs (a -> b) -> Parser a -> RequiredArgs b
