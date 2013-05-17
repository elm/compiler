
module Text where

import Native.Text as T


-- Convert a string into text which can be styled and displayed.
toText : String -> Text

-- Set the typeface of some text. The first argument should be a comma separated listing of the desired typefaces
--     "helvetica, arial, sans-serif"
-- Works the same as the CSS font-family property.
typeface : String -> Text -> Text

-- Switch to a monospace typeface. Good for code snippets.
monospace : Text -> Text

-- Make text big and noticable.
header : Text -> Text

-- Create a link.
link : String -> Text -> Text

-- Set the height of text in \"ems\". 1em is the normal height of text. 2ems is twice that height.
height : Float -> Text -> Text

-- Set the color of a string.
color : Color -> Text -> Text

-- Make a string bold.
bold : Text -> Text

-- Italicize a string.
italic : Text -> Text

-- Draw a line above a string.
overline : Text -> Text

-- Underline a string.
underline : Text -> Text

-- Draw a line through a string.
strikeThrough : Text -> Text

-- Display justified, styled text.
justified : Text -> Element

-- Display centered, styled text.
centered : Text -> Element

-- Display right justified, styled text.
righted : Text -> Element

-- Display styled text.
text : Text -> Element

-- Display a plain string.
plainText : String -> Element

-- Convert anything to it's textual representation and make it displayable in browser
--
--     asText == text . monospace . show
--
-- Excellent for debugging.
asText : a -> Element
