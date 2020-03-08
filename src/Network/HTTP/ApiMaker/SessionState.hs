{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
module Network.HTTP.ApiMaker.SessionState
  ( SessionState(..)
  , Session(..)
  , emptySession
  ) where

import           Control.Lens
import qualified Data.ByteString.Char8 as B
import qualified Network.HTTP.Client   as C


class SessionState st where
  csrfToken :: Lens' st (Maybe B.ByteString)
  sessionData :: Lens' st (Maybe B.ByteString)
  cookieJarData :: Lens' st (Maybe C.CookieJar)

instance SessionState Session where
  csrfToken = lens sessCsrfToken (\sess c' -> sess {sessCsrfToken = c'})
  sessionData = lens sessSessionData (\sess d' -> sess {sessSessionData = d'})
  cookieJarData = lens sessCookieJarData (\sess d' -> sess {sessCookieJarData = d'})

data Session = Session
   --  { unSession :: C.CookieJar }   -- TODO: Does not work? Bug?
  { sessCsrfToken     :: Maybe B.ByteString
  , sessSessionData   :: Maybe B.ByteString
  , sessCookieJarData :: Maybe C.CookieJar
  } deriving (Show)

emptySession :: Session
emptySession = Session Nothing Nothing Nothing

