{- | This module aims to make it easier to create dev tools for Elm.

     It provides the ability to:

       * Detect problems in a program
       * Figure out types of particular expressions
       * Canonicalize variables
       * Convert expressions to JS and HTML

     This module gives limeted access to the structure of an Elm Program.
     Giving full access would expose many internal details that are likely to
     change as Elm progresses. These details have been hidden to avoid breaking
     changes.
-}
module Language.Elm.DevTools where

data Program

-- | Parse an Elm program into an AST.
parse :: String -> Either [Problem] Program

-- | Get the type of a region of the program. Lets you look
--   up the type of particular values and expressions.
typeOf :: Program -> Region -> Either [Problem] String

-- | Canonicalize a variable. So in a normal module `map` would be canonicalized
--   to `List.map`.
canonicalize :: Program -> Region -> [String]

-- | Any problem that may come up while parsing or typing a program.
--   All problems come with a region that is causing the problem.
data Problem = Problem { message :: String, region :: Region }

type Row = Int
type Column = Int

-- | Represents a region, spanning two points in the source code.
data Region = Region { start :: (Row,Column), end :: (Row,Column) }

-- | Takes a specific point in the source code, such as the cursor position.
--   It then determines the region that best represents that point. When the
--   cursor is within a variable, it chooses the variable. When the cursor is
--   on parentheses, it selects the region within the parentheses. When the
--   cursor is on keywords like let, it chooses the whole let-expression.
region :: (Row,Column) -> Region
region pos = Region pos pos
