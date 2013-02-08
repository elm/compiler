title w = size w 60 . text . header . toText $ "Elm-Yesod"

lightGrey = rgb 240 241 244
mediumGrey = rgb 216 221 225
heading outer inner =
  color mediumGrey . size outer 61 . 
  color  lightGrey . size outer 60 . 
  size inner 60 $ title inner

skeleton body outer =
  let inner = if outer < 820 then outer - 20 else 800 in
  flow down [ heading outer inner
            , body outer inner
            ]

----------------------

section = text . bold . Text.height (5/4) . toText

-- splicing of values works but literal substitution of strings 
-- for this reason, we need to have quotes.
mouseLink = "#{mouse}"
clockLink = "#{clock}"
shapesLink = "#{shapes}"


info w = List.map (\f -> f ()) . List.intersperse (\x -> plainText "&nbsp;") . List.map ((\e x -> e) . width w) $
  [ section "Written in Elm, served with Yesod"
  , text $ toText "This page is written in " ++ Text.link "http://elm-lang.org/" (toText "Elm") ++
           toText " and served using the " ++ 
           Text.link "http://yesodweb.com/" (toText "Yesod Web Framework") ++
           toText ". Since you are looking at this page it is safe to assume that you already have the example code. "
  , text $ toText "Type-safe URLs are rendered using simple QuasiQuoter variable interpolation."
  , section "More examples:"
  , text $ toText "- " ++ Text.link mouseLink (toText "A simple mouse input example")
  , text $ toText "- " ++ Text.link clockLink (toText "An animated analog clock")
  , text $ toText "- " ++ Text.link shapesLink (toText "Some simple rendered shapes")
  ]
 
-- The following line does not parse, replaced it something slightly different
-- body outer inner = width outer . flow down $ (:) (plainText "&nbsp;") $ info inner
body outer inner = width outer . flow down $ info inner

main = lift (skeleton body) Window.width
