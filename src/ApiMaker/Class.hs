{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE UndecidableSuperClasses    #-}
module ApiMaker.Class
  ( Request(..)
  , Session(..)
  , Config (..)
  , runReqSafe
  , askConfig
  , askApiConfig
  , ReqSafe (..)
  , headerContentTypeJson
  , headerContentTypeMultipart
  , headerContentDispositionFile
  ) where

import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8      as B
import           Data.Proxy
import qualified Network.HTTP.Client        as C
import           Network.HTTP.Req


data Session = Session
   --  { unSession :: C.CookieJar }   -- TODO: Does not work? Bug?
  { csrfToken     :: Maybe B.ByteString
  , sessionData   :: Maybe B.ByteString
  , cookieJarData :: Maybe C.CookieJar
  } deriving (Show)

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

newtype ReqSafe cfg a =
  ReqSafe (ExceptT HttpException (ReaderT (Config cfg) IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

askConfig :: ReqSafe cfg (Config cfg)
askConfig = ReqSafe (lift ask)

askApiConfig :: ReqSafe cfg cfg
askApiConfig = apiConfig <$> ReqSafe (lift ask)

instance MonadBase IO (ReqSafe cfg) where
  liftBase = liftIO

instance MonadHttp (ReqSafe cfg) where
  handleHttpException = ReqSafe . throwError
  getHttpConfig = httpConfig <$> ReqSafe (lift ask)

runReqSafe ::
     MonadIO m
  => Config cfg                 -- ^ Config including 'HttpConfig' to use
  -> ReqSafe cfg a              -- ^ Computation to run
  -> m (Either HttpException a)
runReqSafe config (ReqSafe m) = liftIO (runReaderT (runExceptT m) config)


headerContentTypeJson :: Option scheme
headerContentTypeJson = header "content-type" "application/json"

headerContentTypeMultipart :: Option scheme
headerContentTypeMultipart = header "content-type" "multipart/form-data"

headerContentDispositionFile :: Text -> Option scheme
headerContentDispositionFile filename = header "Content-Disposition" (E.encodeUtf8 $ T.concat ["attachment; filename=\"", filename, "\""])


-- runReqSafeE ::
--      MonadIO m
--   => Config cfg                 -- ^ Config including 'HttpConfig' to use
--   -> ReqSafe cfg a              -- ^ Computation to run
--   -> ExceptT HttpException m a
-- runReqSafeE = hoistExcept . runReqSafe


-- instance MonadBaseControl IO ReqSafe where
--   type StM ReqSafe a = a
--   liftBaseWith f = ReqSafe . ExceptT . ReaderT $ \r -> f (runReqSafe r)
--   {-# INLINEABLE liftBaseWith #-}
--   restoreM       = ReqSafe . ExceptT . ReaderT . const . return
--   {-# INLINEABLE restoreM #-}
