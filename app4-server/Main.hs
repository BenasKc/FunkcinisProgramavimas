{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class(liftIO)
import Data.String.Conversions
import Web.Scotty
import qualified Lib3
import qualified Lib2
import Control.Concurrent ( Chan, readChan, writeChan, newChan )
import GHC.Conc
import GHC.Base


main :: IO ()
main = do
    state <- newTVarIO Lib2.emptyState
    chan <- newChan :: IO (Chan Lib3.StorageOp)
    _ <- forkIO $ Lib3.storageOpLoop chan
    scotty 3000 $ do
        post "/upload" $ do
            b <- body
            case Lib3.parseCommand (cs b) of
                Right (Lib3.SaveCommand, "") -> do
                    result <- liftIO $ Lib3.stateTransition state Lib3.SaveCommand chan 
                    case result of
                        (Right (Just str)) ->
                            text $ cs str
                        (Left str) ->
                            text $ cs str
                        _ -> text "Unknown response"
                Right (Lib3.LoadCommand, "") -> do
                    result <- liftIO $ Lib3.stateTransition state Lib3.LoadCommand chan 
                    case result of
                        (Right (Just str)) ->
                            text $ cs str
                        (Left str) ->
                            text $ cs str
                        _ -> text "Unknown response"
                Right (Lib3.StatementCommand b, "") -> do
                    result <- liftIO $ Lib3.stateTransition state (Lib3.StatementCommand b) chan 
                    case result of
                        (Right (Just str)) ->
                            text $ cs str
                        (Left str) ->
                            text $ cs str
                        _ -> text "Unknown response"
                _ -> do
                    text "Somethin wrong with ya query man"

-- to test:
-- curl -X POST -d "LOAD" http://localhost:3000/upload