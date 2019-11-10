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
module Network.HTTP.ApiMaker.Class
  ( Request(..)
  , Session(..)
  , Config (..)
  , SessionState (..)
  , runSafeReqM
  , askConfig
  , askApiConfig
  , SafeReq
  , SafeReqM (..)
  , headerContentTypeJson
  , headerContentTypeMultipart
  , headerContentDispositionFile
  ) where

import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8      as B
import           Data.Proxy
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as E
import qualified Network.HTTP.Client        as C
import           Network.HTTP.Req


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

class (HttpMethod (Method r), HttpBody (Body r), HttpResponse (Response r), HttpBodyAllowed (AllowsBody (Method r)) (ProvidesBody (Body r))) =>
      Request cfg r
  where
  type Method r :: *
  type Body r :: *
  type Response r :: *
  type Output r :: *
  method   :: cfg -> r -> Method r
  url      :: cfg -> r -> Url 'Https
  body     :: cfg -> r -> Body r
  response :: cfg -> r -> Proxy (Response r)
  option   :: cfg -> r -> Option 'Https
  process  :: (MonadHttp m) => cfg -> r -> Response r -> StateT Session m (Output r)


-- Type safe request

data Config cfg = Config
  { httpConfig           :: HttpConfig
  , apiDefaultParameters :: [Option 'Https]
  , apiConfig            :: cfg
  }


type SafeReq cfg a = StateT Session (SafeReqM cfg) a

newtype SafeReqM cfg a =
  SafeReqM (ExceptT HttpException (ReaderT (Config cfg) IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

askConfig :: SafeReqM cfg (Config cfg)
askConfig = SafeReqM (lift ask)

askApiConfig :: SafeReqM cfg cfg
askApiConfig = apiConfig <$> SafeReqM (lift ask)

instance MonadBase IO (SafeReqM cfg) where
  liftBase = liftIO

instance MonadHttp (SafeReqM cfg) where
  handleHttpException = SafeReqM . throwError
  getHttpConfig = httpConfig <$> SafeReqM (lift ask)

runSafeReqM ::
     MonadIO m
  => Config cfg                 -- ^ Config including 'HttpConfig' to use
  -> SafeReqM cfg a             -- ^ Computation to run
  -> m (Either HttpException a)
runSafeReqM config (SafeReqM m) = liftIO (runReaderT (runExceptT m) config)


headerContentTypeJson :: Option scheme
headerContentTypeJson = header "content-type" "application/json"

headerContentTypeMultipart :: Option scheme
headerContentTypeMultipart = header "content-type" "multipart/form-data"

headerContentDispositionFile :: T.Text -> Option scheme
headerContentDispositionFile filename = header "Content-Disposition" (E.encodeUtf8 $ T.concat ["attachment; filename=\"", filename, "\""])


-- runSafeReqME ::
--      MonadIO m
--   => Config cfg                 -- ^ Config including 'HttpConfig' to use
--   -> SafeReqM cfg a              -- ^ Computation to run
--   -> ExceptT HttpException m a
-- runSafeReqME = hoistExcept . runSafeReqM


-- instance MonadBaseControl IO SafeReqM where
--   type StM SafeReqM a = a
--   liftBaseWith f = SafeReqM . ExceptT . ReaderT $ \r -> f (runSafeReqM r)
--   {-# INLINEABLE liftBaseWith #-}
--   restoreM       = SafeReqM . ExceptT . ReaderT . const . return
--   {-# INLINEABLE restoreM #-}
