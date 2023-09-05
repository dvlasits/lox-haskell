{-# LANGUAGE NoImplicitPrelude, LambdaCase ,OverloadedStrings, DeriveAnyClass, TemplateHaskell, RecordWildCards, FlexibleInstances, TupleSections, FlexibleContexts, Arrows #-}

module Scanner where

import BasePrelude hiding (first, EOF)
import Control.Exception()
import Control.Monad.Loops
import Control.Monad.Trans.State.Strict
import Control.Lens hiding (noneOf, both)
import Control.Lens.TH
import Control.Lens.Traversal hiding (both)
import TokenType
import Env

data ScanEnv = ScanEnv {
    _env :: MyState
    , _source :: String
    , _start :: Int 
    , _current :: Int
    , _line :: Int
}

makeLenses ''ScanEnv

createScanEnv :: MyState -> String -> ScanEnv 
createScanEnv env source = ScanEnv {_env = env, _source = source, _start = 0, _current = 0, _line = 1}



--- whileJust :: Monad m => m (Maybe a) -> (a -> m b) -> m [b]
-- We have tokens we want to output
scan :: String -> StateIO [Token]
scan source = do 
        mystate <- get 
        (out, state) <- liftIO $ runStateT scan' (createScanEnv mystate source)
        put (view env state)
        return out

isAtEnd :: StateT ScanEnv IO Bool 
isAtEnd = do 
        cur <- gets (view current)
        source <- gets (view source)
        return (cur >= length source)

advance :: StateT ScanEnv IO Char
advance = do 
        current' <- gets (view current)
        current %= (+1)
        gets (fromJust . preview (source . ix current'))


addToken :: TokenType -> StateT ScanEnv IO Token
addToken typet = addToken' typet Null


addToken' :: TokenType -> Object -> StateT ScanEnv IO Token
addToken' typet lit  = do 
                ScanEnv {..} <- get
                let substring = take (_current - _start) . drop _start $ _source
                return $ createToken typet substring lit _line


scanToken = do 
        c <- advance 
        case c of 
            '(' -> addToken LEFT_PAREN
            ')' -> addToken RIGHT_PAREN



scan' :: StateT ScanEnv IO [Token]
scan' = do 
        toks <- flip untilM isAtEnd (do 
                    curr <- gets (view current)
                    start .= curr 
                    scanToken)
        line <- gets (view Scanner.line)
        let endTok = createToken EOF "" Null line
        return (toks ++ [endTok])



