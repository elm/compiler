{-# LANGUAGE KindSignatures, MagicHash, PolyKinds, Rank2Types, TemplateHaskell #-}
module Crash
  ( recover
  --
  , Crash
  , crash
  , crashIO
  , blank
  --
  , Module
  , Name
  , Line
  , crashable
  , crash_
  --
  , E.bracket
  , E.bracket_
  , E.bracketOnError
  , bracketOnError_
  , E.assert
  , E.mask
  , E.mask_
  , E.evaluate
  , E.finally
  , E.onException
  )
  where


import qualified Control.Exception as E
import GHC.IO (IO(IO), unIO)
import GHC.Prim
import Language.Haskell.TH.Syntax (Q, Exp(..), Lit(..), Pat(..))
import qualified Language.Haskell.TH.Syntax as TH



-- RECOVER


recover :: IO a -> (E.SomeException -> IO a) -> IO a
recover (IO work) k =
  IO $ catch# work (unIO . k)



-- CRASH


crash :: TH.Name -> Q Exp -- forall (r :: RuntimeRep). forall (a :: TYPE r). String -> a
crash name =
  do  loc <- TH.qLocation
      msg <- TH.qNewName "msg"
      pure $ LamE [VarP msg] $
        VarE 'E.throw `AppE`
        (
          ConE 'Crash
            `AppE` moduleE loc
            `AppE` nameE name
            `AppE` lineE loc
            `AppE` VarE msg
        )


crashIO :: TH.Name -> Q Exp -- String -> IO a
crashIO name =
  do  loc <- TH.qLocation
      msg <- TH.qNewName "msg"
      pure $ LamE [VarP msg] $
        VarE 'E.throwIO `AppE`
        (
          ConE 'Crash
            `AppE` moduleE loc
            `AppE` nameE name
            `AppE` lineE loc
            `AppE` VarE msg
        )


blank :: TH.Name -> Q Exp -- forall (r :: RuntimeRep). forall (a :: TYPE r). a
blank name =
  do  loc <- TH.qLocation
      pure $
        VarE 'E.throw `AppE`
        (
          ConE 'Crash
            `AppE` moduleE loc
            `AppE` nameE name
            `AppE` lineE loc
            `AppE` LitE (StringL "blank")
        )


crashable :: TH.Name -> TH.Name -> Q Exp
crashable func name =
  do  loc <- TH.qLocation
      pure $
        VarE func
          `AppE` moduleE loc
          `AppE` nameE name
          `AppE` lineE loc


crash_ :: Module -> Name -> Line -> String -> a
crash_ modul name line msg =
  E.throw $ Crash modul name line msg



-- EXCEPTIONS


data Crash =
  Crash
    { _module  :: Module
    , _name    :: Name
    , _line    :: Line
    , _message :: String
    }
  deriving (Show)


instance E.Exception Crash


newtype Module = Module String deriving (Show)
newtype Name   = Name   String deriving (Show)
newtype Line   = Line   Int    deriving (Show)


moduleE :: TH.Loc -> Exp
moduleE loc =
  ConE 'Module `AppE` LitE (StringL (TH.loc_module loc))


nameE :: TH.Name -> Exp
nameE name =
  ConE 'Name `AppE` LitE (StringL (TH.showName name))


lineE :: TH.Loc -> Exp
lineE loc =
  ConE 'Line `AppE` LitE (IntegerL (fromIntegral (fst (TH.loc_start  loc))))



-- BRACKET ON ERROR_


bracketOnError_ :: IO a -> IO b -> IO c -> IO c
bracketOnError_ setup recovery work =
  E.mask $ \restore ->
    do  _ <- setup
        restore work `E.onException` recovery

