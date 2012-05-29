title w = size w 60 . box 4 . text . header . toText $ "Elm-Yesod"

lightGrey = rgb (240/255) (241/255) (244/255)
mediumGrey = rgb (216/255) (221/255) (225/255)
heading outer inner =
  color mediumGrey . size outer 61 . box 1 .
  color  lightGrey . size outer 60 . box 5 .
  size inner 60 . box 5 $ title inner

skeleton body outer =
  let inner = if outer < 820 then outer - 20 else 800 in
  flow down [ heading outer inner
            , body outer inner
            ]

----------------------

section = text . bold . Text.height (5/4) . toText

info w = List.map (\f -> f ()) . List.intersperse (\x -> plainText "&nbsp;") . List.map ((\e x -> e) . width w) $
  [ section "Written in Elm, served with Yesod"
  , text $ toText "This page is written in " ++ link "http://elm-lang.org/" (toText "Elm") ++
           toText " and served using the " ++ 
           link "http://yesodweb.com/" (toText "Yesod Web Framework") ++
           toText ". Since you are looking at this page it is safe to assume that you already have the example code. "
  , text $ toText "Rendering Yesod's type-safe URLs is not yet possible, but will be implemented as the currently " ++
           toText "very simple QuasiQuoter is extended."
  , section "More examples:"
  , text $ toText "- " ++ link ^{mouse} (toText "A simple mouse input example")
  , text $ toText "- " ++ link ^{clock} (toText "An animated analog clock")
  , text $ toText "- " ++ link ^{shapes} (toText "Some simple rendered shapes")
  ]
 
body outer inner = width outer . box 2 . flow down . (:) (plainText "&nbsp;") $ info inner

main = lift (skeleton body) Window.width
