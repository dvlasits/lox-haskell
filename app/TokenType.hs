{-# LANGUAGE NoImplicitPrelude, LambdaCase ,OverloadedStrings, DeriveAnyClass, TemplateHaskell, RecordWildCards, FlexibleInstances, TupleSections, FlexibleContexts, Arrows #-}


module TokenType where

import BasePrelude hiding (first)
import Control.Exception()
import Control.Monad.Loops
import Control.Monad.Trans.State.Strict
import Control.Lens hiding (noneOf, both)
import Control.Lens.TH
import Control.Lens.Traversal hiding (both)

data Object = Null | S String | N Float deriving (Show)

data Token = Token {
      _typet :: TokenType
    , _lexeme :: String
    , _literal :: Object -- Not sure what to represent a literal as yet? 
    , _line :: Int
} deriving (Show)

createToken :: TokenType -> String -> Object -> Int -> Token
createToken _typet _lexeme _literal _line = Token { .. }

data TokenType = -- Single Character
                LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE
                | COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH
                | STAR 
                -- One or two character tokens 
                | BANG | BANG_EQUAL | EQUAL | EQUAL_EQUAL
                | GREATER | GREATER_EQUAL | LESS | LESS_EQUAL
                -- Literals 
                | IDENTIFIER | STRING | NUMBER 
                -- Keywords
                | AND | CLASS | ELSE | FALSE | FUN | FOR | IF 
                | NIL | OR | PRINT | RETURN | SUPER | THIS | TRUE
                | VAR | WHILE | EOF deriving (Show)

makeLenses ''Token