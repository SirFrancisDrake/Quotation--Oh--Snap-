{-# LANGUAGE TemplateHaskell #-}

{-

This module defines our application's state type and an alias for its handler
monad.

-}

module Application where

import Data.Lens.Template
import Data.Time.Clock

import Snap.Snaplet
import Snap.Snaplet.Heist

data App = App
    { _heist :: Snaplet (Heist App)
    , _startTime :: UTCTime
    , _authLens :: Snaplet (AuthManager App)
    , _sessLens :: Snaplet SessionManager
    , _dbLens :: Snaplet (HdbcSnaplet Connection IO)
    }

type AppHandler = Handler App App

makeLens ''App

instance HasHeist App where
    heistLens = subSnaplet heist

