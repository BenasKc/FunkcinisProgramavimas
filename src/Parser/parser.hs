import Data.Either (partitionEithers)
import Basics
import System.IO (hFlush, stdout)
import Control.Monad (unless)

--main :: IO ()
--main = do
--  putStrLn "Welcome to the Hospital Management System"
--  putStrLn "Enter a command (REGISTER PATIENT ..., BOOK APPOINTMENT ..., UPDATE PATIENT ..., SEARCH PATIENT ...)"
 -- putStrLn "Enter 'QUIT' to exit."

main :: IO ()
main = loop initialHospitalState
  where
    loop :: HospitalState -> IO ()
    loop state = do
      putStrLn "Enter a command: "
      commandStr <- getLine
      case parseAndExecuteCommand state commandStr of
        Left errMsg -> do
          putStrLn $ "Error: " ++ errMsg
          loop state  -- Retry with the same state
        Right (newState, resultMsg) -> do
          putStrLn resultMsg
          loop newState  -- Pass the updated state to the next loop iteration


--commandLoop :: IO ()
--commandLoop = do
--  putStr "> "
 -- hFlush stdout
 -- command <- getLine
 -- unless (command == "QUIT") $ do
  --  let result = parseAndExecuteCommand command
   -- putStrLn $ case result of
    --  Left err -> "Error: " ++ err
     -- Right output -> "Result: " ++ show output
   -- commandLoop

parseAndExecuteCommand :: HospitalState -> String -> Either String (HospitalState, String)
parseAndExecuteCommand hs str = do
  command <- parseCommand str  -- Parse the command
  executeCommand hs command    -- Execute the command with the current state