{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    parseSequentially,
    parseUntilSemicolon
    ) where

import Control.Concurrent ( Chan, readChan, writeChan, newChan )
import Control.Concurrent.STM(STM, TVar, readTVar, atomically, modifyTVar', writeTVar)
import Control.Exception (try, SomeException)
import qualified Lib2
import Data.List (stripPrefix)
import Lib2 (parseNWords)

data StorageOp = Save String (Chan ()) | Load (Chan String)
-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = loop
  where 
    loop :: IO()
    loop = do
      op <- readChan chan
      case op of
        Save content responseChan -> do
          result <- try (writeFile "state.txt" content) :: IO (Either SomeException ())
          case result of
            Right _ -> writeChan responseChan ()
            Left _ -> putStrLn "Failed to save state to file"
          loop

        Load responseChan -> do
          result <- try (readFile "state.txt") :: IO (Either SomeException String)
          case result of
            Right content -> writeChan responseChan content
            Left _        -> writeChan responseChan ""      -- send empty str
          loop

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand s = case Lib2.parseNWords s 1 of
  (["SAVE"], _)          -> Right (SaveCommand, "")
  (["LOAD"], _)          -> Right (LoadCommand, "")
  (["BEGIN"], x)         -> 
    case parseStatements x of
      Right (stats, remains) -> 
        case parseNWords remains 1 of
          (["END"], "") -> Right (StatementCommand stats, "")
          _      -> Left remains
      Left err               -> Left err
  _               -> Left "Unknown command_"
    

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements str = 
  case parseSequentially str Nothing of
    Right (stats, rems) -> Right (stats, rems)
    Left err            -> Left err


parseSequentially :: String -> Maybe Statements -> Either String (Statements, String)
parseSequentially str stats =
  case stats of
    Just (Single query)  -> 
      case parseUntilSemicolon str of
        Right (_stats, remains) -> parseSequentially remains (Just (Batch ([query] ++ [_stats])))
        Left rem                       -> Right (Single query, rem)
        _                              -> Left "Unknown err" 

    Just (Batch queries) ->
      case parseUntilSemicolon str of
        Right (_stats, remains) -> parseSequentially remains (Just (Batch (queries ++ [_stats])))
        Left rem                       -> Right (Batch queries, rem)
        _                              -> Left "Unknown err" 

    Nothing              ->
      case parseUntilSemicolon str of
        Right (_stats, remains) -> parseSequentially remains (Just (Single _stats))
        Left rem                       -> Left "No valid statements"
        _                              -> Left "Unknown err" 

parseUntilSemicolon :: String -> Either String (Lib2.Query, String)
parseUntilSemicolon str = 
  case Lib2.parseQueryRem str of
    Right (q, xs) -> Right (q, xs)
    Right (q, [])   -> Right (q, "")
    _                 -> Left str

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState state =
  let queries = stateToQueries state 
  in case queries of
       [q] -> Single q
       qs  -> Batch qs

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single query) = renderQuery query ++ ";"
renderStatements (Batch queries) =
  unlines (map (\q -> renderQuery q ++ ";") queries)

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable

stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String))
stateTransition stateVar cmd ioChan = case cmd of

    StatementCommand statements -> do
        let queries = case statements of
                        Single q  -> [q]
                        Batch qs  -> qs
        result <- atomically $ do
            st <- readTVar stateVar
            case applyQueries (Right ("", st)) queries of
                Right (msg, newState) -> do
                    writeTVar stateVar newState
                    return $ Right msg
                Left err -> return $ Left err
        case result of
            Right msg -> do
                putStrLn $ "Query(-ies) executed: " ++ msg
                return $ Right $ Just $ "Query(-ies) successfully executed: " ++ msg
            Left err -> do
                putStrLn $ "Failed to execute queries: " ++ err
                return $ Left err

    SaveCommand -> do
        currentState <- atomically $ readTVar stateVar
        let serializedState = renderStatements $ marshallState currentState
        responseChan <- newChan
        writeChan ioChan (Save serializedState responseChan)
        _ <- readChan responseChan
        return $ Right $ Just "State saved successfully."

    LoadCommand -> do
        responseChan <- newChan
        writeChan ioChan (Load responseChan)
        loadedState <- readChan responseChan
        case parseStatements loadedState of
            Right (parsedStatements, "") -> do
                let newState = case applyQueries (Right ("", Lib2.emptyState)) (toQueries parsedStatements) of
                                 Right (_, st) -> st
                                 Left _        -> Lib2.emptyState
                atomically $ writeTVar stateVar newState
                return $ Right $ Just "State loaded successfully."
            Right (_, extra) -> do
                putStrLn $ "Error: Extra data in loaded state: " ++ extra
                return $ Left extra
            Left err -> do
                putStrLn $ "Failed to load state: " ++ err
                return $ Left $ "Failed to load state: " ++ err
  where
    toQueries (Single q) = [q]
    toQueries (Batch qs) = qs

applyQueries :: Either String (String, Lib2.State) -> [Lib2.Query] -> Either String (String, Lib2.State)
applyQueries initialState queries = foldl applyQuery initialState queries
  where
    applyQuery :: Either String (String, Lib2.State) -> Lib2.Query -> Either String (String, Lib2.State)
    applyQuery (Left err) _ = Left err
    applyQuery (Right (msg, state)) query =
      case Lib2.stateTransition state query of
        Left err -> Left err 
        Right (Just newMsg, newState) ->
          Right (msg ++ "\n" ++ newMsg, newState)
        Right (_, newState) ->
          Right (msg, newState)

renderQuery :: Lib2.Query -> String
renderQuery (Lib2.RegisterQuery (Lib2.PatientInfo patientId patientName patientAge patientGender patientContacts patientAddress)) =
    "REGISTER PATIENT " ++ Lib2.pid2str patientId ++ " " ++ Lib2.name2str (Right patientName) ++ " " ++ Lib2.age2str patientAge ++ " " ++ Lib2.gender2str (Right patientGender) ++  " " ++ Lib2.addr2str patientAddress ++ " " ++ Lib2.ci2str (Right patientContacts) 

renderQuery (Lib2.BookAppointmentQuery (Lib2.Appointment appointmentPatientId appointmentDoctorName appointmentDepartment appointmentDate appointmentTime)) =
    "BOOK APPOINTMENT " ++ Lib2.pid2str appointmentPatientId ++ " " ++ Lib2.doctorName2str (Right appointmentDoctorName) ++ " " ++ Lib2.deptToStr appointmentDepartment ++ " " ++ Lib2.formatDate appointmentDate ++ " " ++ Lib2.time2str appointmentTime

renderQuery _ = ""

stateToQueries :: Lib2.State -> [Lib2.Query]
stateToQueries (Lib2.HospitalState appointments patientInfos) =
    let
        patientQueries = map Lib2.RegisterQuery patientInfos
        appointmentQueries = map Lib2.BookAppointmentQuery appointments
    in
        patientQueries ++ appointmentQueries

-- <*> is an applicative
-- <$> is a functor
-- 