module Type.Hint where

import Control.Applicative ( (<$>) )
import qualified Data.UnionFind.IO as UF
import Text.PrettyPrint as P

import qualified AST.Annotation as A
import AST.PrettyPrint (pretty)
import Type.Type (Descriptor, toSrcType)


type Hint = Doc


create
    :: A.Region
    -> Maybe String
    -> UF.Point Descriptor
    -> UF.Point Descriptor
    -> IO Hint
create region hint t1 t2 =
  do  t1' <- pretty <$> toSrcType t1
      t2' <- pretty <$> toSrcType t2
      return . foldr ($+$) P.empty $
         [ P.text "Type mismatch between the following types" <+> pretty region <> P.text ":"
         , P.text ""
         , P.nest 8 t1'
         , P.text ""
         , P.nest 8 t2'
         , P.text ""
         , maybe P.empty (P.nest 4 . P.text) hint
         , P.text "    It is related to the following expression:"
         , P.text ""
         , P.nest 8 $ A.getRegionDocs region
         ]