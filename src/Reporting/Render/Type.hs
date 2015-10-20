module Reporting.Render.Type
  ( Localizer
  , typeDecl
  , aliasDecl
  , annotation
  , tipe
  )
  where

import qualified Data.Map as Map
import Text.PrettyPrint.ANSI.Leijen

import qualified AST.Helpers as Help


type Localizer =
  Map.Map String String


typeDecl =
  error "TODO"


aliasDecl =
  error "TODO"


annotation =
  error "TODO"


tipe =
  error "TODO"



{-- HELPERS

commaCat :: [Doc] -> Doc
commaCat docs =
  cat (punctuate comma docs)


commaSep :: [Doc] -> Doc
commaSep docs =
  sep (punctuate comma docs)


parensIf :: Bool -> Doc -> Doc
parensIf bool doc =
  if bool then parens doc else doc


variable :: String -> Doc
variable x =
  if Help.isOp x then parens (text x) else text x



-- PRETTY PRINTING

instance (P.Pretty t) => P.Pretty (Port t) where
  pretty dealiaser needsParens portType =
    P.pretty dealiaser needsParens (getPortType portType)


instance P.Pretty Raw' where
  pretty dealiaser needsParens tipe =
    case tipe of
      RLambda arg body ->
          P.parensIf needsParens (prettyLambda dealiaser getRawLambda arg body)

      RVar x ->
          P.text x

      RType var ->
          prettyType dealiaser var

      RApp func args ->
          let
            isTuple (A.A _ (RType name)) = Help.isTuple (Var.toString name)
            isTuple _ = False
          in
            prettyApp dealiaser needsParens isTuple func args

      RRecord fields ext ->
          prettyRecord dealiaser (flattenRawRecord fields ext)


instance P.Pretty Canonical where
  pretty dealiaser needsParens tipe =
    case tipe of
      Lambda arg body ->
          P.parensIf needsParens (prettyLambda dealiaser getCanLambda arg body)

      Var x ->
          P.text x

      Type var ->
          prettyType dealiaser var

      App func args ->
          let
            isTuple (Type name) = Help.isTuple (Var.toString name)
            isTuple _ = False
          in
            prettyApp dealiaser needsParens isTuple func args

      Record fields ext ->
          prettyRecord dealiaser (flattenCanRecord fields ext)

      Aliased name args _ ->
          P.parensIf (needsParens && not (null args)) $
            P.hang
              (P.pretty dealiaser False name)
              2
              (P.sep (map (P.pretty dealiaser True . snd) args))


-- PRETTY HELPERS

prettyType :: (Var.ToString var) => P.Dealiaser -> var -> P.Doc
prettyType dealiaser var =
  let
    v = Var.toString var
  in
    P.text $
      if v == "_Tuple0" then
        "()"
      else
        maybe v id (Map.lookup v dealiaser)


-- PRETTY LAMBDAS

prettyLambda :: (P.Pretty t) => P.Dealiaser -> (t -> Maybe (t,t)) -> t -> t -> P.Doc
prettyLambda dealiaser getLambda arg body =
  let
    rest =
      gatherLambda getLambda body

    prettyArg t =
      P.pretty dealiaser (Maybe.isJust (getLambda t)) t
  in
    P.sep
      [ prettyArg arg
      , P.sep (map (\t -> P.text "->" <+> prettyArg t) rest)
      ]


getRawLambda :: Raw -> Maybe (Raw, Raw)
getRawLambda (A.A _ tipe) =
  case tipe of
    RLambda arg body -> Just (arg, body)
    _ -> Nothing


getCanLambda :: Canonical -> Maybe (Canonical, Canonical)
getCanLambda tipe =
  case tipe of
    Lambda arg body -> Just (arg, body)
    _ -> Nothing


gatherLambda :: (t -> Maybe (t,t)) -> t -> [t]
gatherLambda get tipe =
  case get tipe of
    Just (arg, body) ->
        arg : gatherLambda get body

    Nothing ->
        [tipe]


collectLambdas :: Canonical -> [Canonical]
collectLambdas tipe =
  gatherLambda getCanLambda tipe


-- PRETTY APP

prettyApp :: (P.Pretty t) => P.Dealiaser -> Bool -> (t -> Bool) -> t -> [t] -> P.Doc
prettyApp dealiaser needsParens isTuple func args
  | isTuple func =
        P.parens $ P.sep $
            P.punctuate P.comma (map (P.pretty dealiaser False) args)

  | null args =
      P.pretty dealiaser needsParens func

  | otherwise =
      P.parensIf needsParens $
        P.hang
          (P.pretty dealiaser True func)
          2
          (P.sep (map (P.pretty dealiaser True) args))


-- PRETTY RECORD

prettyRecord :: (P.Pretty t) => P.Dealiaser -> ([(String, t)], Maybe String) -> P.Doc
prettyRecord dealiaser recordInfo =
  let
    prettyField (field, tipe) =
      P.hang
          (P.text field <+> P.text ":")
          4
          (P.pretty dealiaser False tipe)
  in
  case recordInfo of
    ([], Nothing) ->
        P.text "{}"

    (fields, Nothing) ->
        P.sep
          [ P.cat (zipWith (<+>) (P.lbrace : repeat P.comma) (map prettyField fields))
          , P.rbrace
          ]

    (fields, Just x) ->
        P.hang
            (P.lbrace <+> P.text x <+> P.text "|")
            4
            (P.sep
              [ P.sep (P.punctuate P.comma (map prettyField fields))
              , P.rbrace
              ]
            )


flattenRawRecord
    :: [(String, Raw)]
    -> Maybe Raw
    -> ( [(String, Raw)], Maybe String )
flattenRawRecord fields ext =
  case ext of
    Nothing ->
        (fields, Nothing)

    Just (A.A _ (RVar x)) ->
        (fields, Just x)

    Just (A.A _ (RRecord fields' ext')) ->
        flattenRawRecord (fields' ++ fields) ext'

    _ ->
        error "Trying to flatten ill-formed record."


flattenCanRecord
    :: [(String, Canonical)]
    -> Maybe Canonical
    -> ( [(String, Canonical)], Maybe String )
flattenCanRecord fields ext =
  case ext of
    Nothing ->
        (fields, Nothing)

    Just (Var x) ->
        (fields, Just x)

    Just (Record fields' ext') ->
        flattenCanRecord (fields' ++ fields) ext'

    Just (Aliased _ args tipe) ->
        flattenCanRecord fields (Just (dealias args tipe))

    _ ->
        error "Trying to flatten ill-formed record."
--}