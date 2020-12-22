{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Network.HTTP.ApiMaker.SessionState
  ( SessionState(..)
  , Session(..)
  , emptySession
  ) where

import           Control.Lens
import qualified Data.ByteString.Char8 as B
import qualified Network.HTTP.Client   as C

-- | Session state contract.
class SessionState st where
  csrfToken :: Lens' st (Maybe B.ByteString)
  sessionData :: Lens' st (Maybe B.ByteString)
  cookieJarData :: Lens' st (Maybe C.CookieJar)


-- | Simple session state. This probably is sufficient for the day-to-day use.
data Session = Session
   --  { unSession :: C.CookieJar }   -- TODO: Does not work? Bug in req library?
  { sessCsrfToken     :: Maybe B.ByteString
  , sessSessionData   :: Maybe B.ByteString
  , sessCookieJarData :: Maybe C.CookieJar
  } deriving (Show)

-- | Simple session state implemention.
instance SessionState Session where
  csrfToken = lens sessCsrfToken (\sess c' -> sess {sessCsrfToken = c'})
  sessionData = lens sessSessionData (\sess d' -> sess {sessSessionData = d'})
  cookieJarData = lens sessCookieJarData (\sess d' -> sess {sessCookieJarData = d'})


-- | Empty session state.
emptySession :: Session
emptySession = Session Nothing Nothing Nothing

