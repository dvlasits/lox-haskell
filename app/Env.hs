{-# LANGUAGE NoImplicitPrelude, LambdaCase ,OverloadedStrings, DeriveAnyClass, TemplateHaskell, RecordWildCards, FlexibleInstances, TupleSections, FlexibleContexts, Arrows #-}


module Env where 

import BasePrelude hiding (first)
import Control.Exception()
import Control.Monad.Loops
import Control.Monad.Trans.State.Strict
import Control.Lens hiding (noneOf, both)
import Control.Lens.TH
import Control.Lens.Traversal hiding (both)


newtype MyException = E String deriving (Show)
instance Exception MyException

data MyState = MyState {_hadError :: Bool} deriving (Show)

makeLenses ''MyState

startState = MyState {
       _hadError = False
}

lprint :: Show a => a -> StateIO ()
lprint = liftIO . print

lthrowIO :: Exception e => e -> StateIO a
lthrowIO = liftIO . throwIO

type StateIO = StateT MyState IO