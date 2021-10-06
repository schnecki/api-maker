{-# LANGUAGE TemplateHaskell #-}
module Network.HTTP.ApiMaker.Logging
    ( enableRequestLogging
    , disableRequestLogging
    ) where

import           EasyLogger

enableRequestLogging :: LogDestination -> LogLevel -> IO ()
enableRequestLogging = $(initLogger)

disableRequestLogging :: IO ()
disableRequestLogging = $(finalizeLogger)
