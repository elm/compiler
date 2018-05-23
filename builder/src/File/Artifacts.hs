module File.Artifacts
  ( ignore
  , write
  , writeDocs
  )
  where


import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Monad (foldM, void)
import Control.Monad.Except (liftIO)
import qualified Data.Binary as Binary
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text.Encoding as Text

import qualified Elm.Compiler as Compiler
import qualified Elm.Compiler.Module as Module
import qualified Elm.Docs as Docs
import qualified Json.Encode as Encode

import File.Compile (Answer(..))
import qualified Reporting.Exit.Compile as E
import qualified Reporting.Exit as Exit
import qualified Reporting.Task as Task
import qualified Stuff.Paths as Path



-- IGNORE


ignore :: Map Module.Raw Answer -> Task.Task (Map Module.Raw Compiler.Artifacts)
ignore answers =
  let
    ignorer _name result =
      return result
  in
    gather ignorer answers



-- WRITE


write :: FilePath -> Map Module.Raw Answer -> Task.Task (Map Module.Raw Compiler.Artifacts)
write root answers =
  let
    writer name result@(Compiler.Artifacts elmi elmo _) =
      do  mvar <- newEmptyMVar
          void $ forkIO $
            do  Binary.encodeFile (Path.elmi root name) elmi
                Binary.encodeFile (Path.elmo root name) elmo
                putMVar mvar result
          return mvar
  in
    do  mvars <- gather writer answers
        liftIO $ traverse readMVar mvars


writeDocs :: Map Module.Raw Compiler.Artifacts -> FilePath -> Task.Task Docs.Documentation
writeDocs results path =
  let
    getDocs (Compiler.Artifacts _ _ docs) =
      docs
  in
    case Maybe.mapMaybe getDocs (Map.elems results) of
      [] ->
        return Map.empty

      docs ->
        do  liftIO $ Encode.writeUgly path $ Encode.list Docs.encode docs
            return $ Docs.toDict docs



-- GATHER


gather :: OnGood a -> Map Module.Raw Answer -> Task.Task (Map Module.Raw a)
gather onGood answers =
  do  summary <- liftIO $
        foldM (gatherHelp onGood) (Right Map.empty) (Map.toList answers)

      case summary of
        Left (err, errors) ->
          Task.throw (Exit.Compile err errors)

        Right results ->
          return results


type OnGood a = Module.Raw -> Compiler.Artifacts -> IO a


gatherHelp :: OnGood a -> Summary a -> (Module.Raw, Answer) -> IO (Summary a)
gatherHelp onGood summary (name, answer) =
  case answer of
    Blocked ->
      return summary

    Bad path time src errors ->
      do  let err = E.Exit name path time (Text.decodeUtf8 src) errors
          return (addErr err summary)

    Good result ->
      do  value <- onGood name result
          return (addOk name value summary)



-- DICT RESULT


type Summary a =
  Either (E.Exit, [E.Exit]) (Map Module.Raw a)


addOk :: Module.Raw -> a -> Summary a -> Summary a
addOk name result acc =
  case acc of
    Left _ ->
      acc

    Right results ->
      Right (Map.insert name result results)


addErr :: E.Exit -> Summary a -> Summary a
addErr err acc =
  case acc of
    Left (e, es) ->
      Left (err, e:es)

    Right _ ->
      Left (err, [])
