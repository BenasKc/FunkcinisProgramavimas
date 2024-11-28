{-# LANGUAGE DeriveFunctor #-}
module Main (main) where
    
import Control.Monad.Free (Free (..), liftF)
import Data.ByteString ( ByteString )
import Network.Wreq
import Data.String.Conversions
import Control.Lens


data HospitalRegistrationAlgebra next = 
      Load (() -> next)
    | Save (() -> next)
    | Batch  [String] (() -> next)
    deriving Functor

type HospitalRegDomain = Free HospitalRegistrationAlgebra

load :: HospitalRegDomain ()
load = liftF $ Load id

save :: HospitalRegDomain ()
save = liftF $ Save id

batch :: [String] -> HospitalRegDomain ()
batch queries = liftF $ Batch queries id

interpretSingle :: HospitalRegDomain a -> IO a
interpretSingle (Pure a) = return a
interpretSingle (Free cmd) = do
    case cmd of
        Load next -> do
            putStrLn "Sending LOAD command..."
            resp <- post "http://localhost:3000/upload" (cs "LOAD" :: ByteString)
            putStrLn $ "Response: " ++ cs (resp ^. responseBody)
            interpretSingle (next ())
        Save next -> do
            putStrLn "Sending SAVE command..."
            resp <- post "http://localhost:3000/upload" (cs "SAVE" :: ByteString)
            putStrLn $ "Response: " ++ cs (resp ^. responseBody)
            interpretSingle (next ())
        Batch cmds next -> do
            putStrLn "Sending BATCH commands..."
            let body = cs $ ("BEGIN " ++ unlines cmds ++ " END") :: ByteString
            resp <- post "http://localhost:3000/upload" (cs body :: ByteString)
            putStrLn $ "Response: " ++ cs (resp ^. responseBody)
            interpretSingle (next ())

interpretBatch :: HospitalRegDomain a -> IO a
interpretBatch = go []
  where
    go :: [String] -> HospitalRegDomain a -> IO a
    go acc (Pure a) = do
        flushBatch acc 
        return a
    go acc (Free cmd) =
        case cmd of
            Load next -> do
                flushBatch acc
                putStrLn "Sending LOAD command..."
                resp <- post "http://localhost:3000/upload" (cs "LOAD" :: ByteString)
                putStrLn $ "Response: " ++ cs (resp ^. responseBody)
                go [] (next ())
            Save next -> do
                flushBatch acc
                putStrLn "Sending SAVE command..."
                resp <- post "http://localhost:3000/upload" (cs "SAVE" :: ByteString)
                putStrLn $ "Response: " ++ cs (resp ^. responseBody)
                go [] (next ())
            Batch cmds next -> do
                go (acc ++ cmds) (next ())

    flushBatch :: [String] -> IO ()
    flushBatch [] = return ()
    flushBatch cmds = do
        putStrLn "Sending accumulated BATCH commands..."
        let body = cs $ ("BEGIN " ++ unlines cmds ++ " END") :: ByteString
        resp <- post "http://localhost:3000/upload" (cs body :: ByteString)
        putStrLn $ "Response: " ++ cs (resp ^. responseBody)


interpretInMemory :: HospitalRegDomain a -> IO a
interpretInMemory (Pure a) = return a
interpretInMemory (Free cmd) = do
   case cmd of
       Load next -> do
        putStrLn "Simulating LOAD in memory..."
        interpretInMemory (next ())
       Save next -> do
           putStrLn "Simulating SAVE in memory..."
           interpretInMemory (next ())
       Batch cmds next -> do
           putStrLn "Simulating BATCH in memory..."
           interpretInMemory (next ())

exampleProgram :: HospitalRegDomain ()
exampleProgram = do
    load
    batch ["VIEW ALL APPOINTMENTS;", "VIEW ALL APPOINTMENTS; "]
    batch ["REGISTER PATIENT 5 John Doe 18 Male Washington DC example@example.com; \
            \ REGISTER PATIENT 6 Luke Reed 45 Male New York 87|6841305; \
            \ SEARCH PATIENT Name = Luke Reed; \
            \ REGISTER PATIENT 7 Martha Bronhs 58 Female Los Angeles oldemail@something.org; \
            \ SEARCH PATIENT Gender = Female;", "UPDATE PATIENT 7 Name Martha Creox;"] 
    save 
    batch ["SEARCH PATIENT Age = 58; \
            \ BOOK APPOINTMENT 5 Dr Matt Newman Neurology 12-08-2024 12:35; \
            \ BOOK APPOINTMENT 5 Dr Abraham Lebz Cardiology 01-04-2025 15:55; \
            \ VIEW ALL APPOINTMENTS; \
            \"]
    save
    load
    batch ["VIEW ALL APPOINTMENTS;"]
    save

main :: IO ()
main = do
    -- testProgram

    putStrLn "Running with Single Command Interpreter:"
    interpretSingle exampleProgram

    --putStrLn "Running with Batching Interpreter:"
    --interpretBatch exampleProgram

testProgram :: IO ()
testProgram = do
    interpretInMemory exampleProgram
    putStrLn "In-Memory Interpretation Successful"