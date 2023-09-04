{-# LANGUAGE NoImplicitPrelude, LambdaCase ,OverloadedStrings, DeriveAnyClass, TemplateHaskell, RecordWildCards, FlexibleInstances, TupleSections, FlexibleContexts, Arrows #-}

module Main where

import BasePrelude hiding (first)
import Control.Exception()
import Control.Monad.Loops
import Control.Monad.Trans.State.Strict
import Control.Lens hiding (noneOf, both)
import Control.Lens.TH
import Control.Lens.Traversal hiding (both)
import TokenType
import qualified Scanner
import Env

main :: IO ()
main = flip evalStateT startState $ do 
                     args <- liftIO getArgs
                     case length args of 
                            0 -> runPrompt
                            1 -> runFile (head args)
                            _ -> liftIO $ throwIO (E "Usage: jlox [script]")

runFile :: String -> StateIO ()
runFile path = do 
       file <- liftIO $ readFile path
       run file
       he <- gets (view hadError)
       when (he == True) (lthrowIO (E "Error Occured"))

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

runPrompt :: StateIO ()
runPrompt = unfoldM_ $ do 
                     input <- liftIO $ prompt ">> "
                     if input == "" then 
                            return Nothing 
                     else do
                          run input 
                          hadError .= False
                          return (Just ())

run :: String -> StateIO ()
run file = do 
       scanned <- Scanner.scan file
       lprint scanned



error :: Int -> String -> StateIO ()
error line message = report line "" message

report :: Int -> String -> String -> StateIO ()
report line wher message = do 
              hadError .= True
              liftIO . print $ "[line " <> show line <> "] Error" <> wher <> ": " <> message
              
-- May need to come back to this and adjust hadError = true
