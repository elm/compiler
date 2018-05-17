{-# LANGUAGE OverloadedStrings #-}
module Reporting.Exit.Make
  ( Exit(..)
  , toReport
  )
  where


import qualified Elm.Compiler.Module as Module
import qualified Reporting.Doc as D
import qualified Reporting.Exit.Help as Help



-- EXITS


data Exit
  = CannotMakeNothing
  | CannotOptimizeDebugValues Module.Raw [Module.Raw]
  | CannotOptimizeAndDebug



-- TO REPORT


toReport :: Exit -> Help.Report
toReport exit =
  case exit of
    CannotMakeNothing ->
      Help.report "NO INPUT" Nothing
        "What should I make though? I need more information, like:"
        [ D.vcat
            [ D.indent 4 $ D.green "elm make src/Main.elm"
            , D.indent 4 $ D.green "elm make src/This.elm src/That.elm"
            ]
        , D.reflow
            "However many files you give, I will create one JS file out of them."
        ]

    CannotOptimizeDebugValues m ms ->
      Help.report "DEBUG REMNANTS" Nothing
        "There are uses of the `Debug` module in the following modules:"
        [ D.indent 4 $ D.red $ D.vcat $ map (D.fromString . Module.nameToString) (m:ms)
        , D.reflow "But the --optimize flag only works if all `Debug` functions are removed!"
        , D.toSimpleNote $
            "The issue is that --optimize strips out info needed by `Debug` functions.\
            \ Here are two examples:"
        , D.indent 4 $ D.reflow $
            "(1) It shortens record field names. This makes the generated JavaScript is\
            \ smaller, but `Debug.toString` cannot know the real field names anymore."
        , D.indent 4 $ D.reflow $
            "(2) Values like `type Height = Height Float` are unboxed. This reduces\
            \ allocation, but it also means that `Debug.toString` cannot tell if it is\
            \ looking at a `Height` or `Float` value."
        , D.reflow $
            "There are a few other cases like that, and it will be much worse once we start\
            \ inlining code. That optimization could move `Debug.log` and `Debug.todo` calls,\
            \ resulting in unpredictable behavior. I hope that clarifies why this restriction\
            \ exists!"
        ]

    CannotOptimizeAndDebug ->
      Help.docReport "CLASHING FLAGS" Nothing
        ( D.fillSep
            ["I","cannot","compile","with",D.red "--optimize","and"
            ,D.red "--debug","at","the","same","time."
            ]
        )
        [ D.reflow
            "I need to take away information to optimize things, and I need to\
            \ add information to add the debugger. It is impossible to do both\
            \ at once, so pick just one of those flags."
        ]