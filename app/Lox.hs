{-# LANGUAGE NoImplicitPrelude, LambdaCase ,OverloadedStrings, DeriveAnyClass, TemplateHaskell, RecordWildCards, FlexibleInstances, TupleSections, FlexibleContexts, Arrows #-}

module Main where

import BasePrelude hiding (first)
import Control.Exception()
import Control.Monad.Loops
import Control.Monad.Trans.State.Lazy

newtype MyException = E String deriving (Show)
instance Exception MyException

data MyState = MyState {hadError :: Bool} 

startState = MyState {
       hadError = False
}

type StateIO = StateT IO

main :: IO ()
main = runStateT startState $ do 
                     args <- liftIO getArgs
                     case length args of 
                            0 -> runPrompt
                            1 -> runFile (head args)
                            _ -> throwIO (E "Usage: jlox [script]")

runFile :: String -> StateIO ()
runFile path = do 
       file <- liftIO $ readFile path
       run file

runPrompt :: StateIO ()
runPrompt = unfoldM_ $ do 
                     putStr "> "
                     input <- liftIO getLine
                     if input == "" then 
                            return Nothing 
                     else 
                          Just <$> run input

run :: String -> StateIO ()
run file = do 
       let scanned = scan file
       liftIO $ print scanned

scan :: String -> [String]
scan = undefined

error :: Int -> String -> StateIO ()
error line message = report line "" message

report :: Int -> String -> String -> IO ()
report line wher message = do 
              hadError .= True
              liftIO $ print "[line " <> show line <> "] Error" <> wher <> ": " <> message
              
-- May need to come back to this and adjust hadError = true