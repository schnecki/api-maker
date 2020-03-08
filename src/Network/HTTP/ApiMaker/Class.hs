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
  , Config (..)
  , SessionState (..)
  , Session(..)
  , emptySession
  , runSafeReqM
  , askConfig
  , askApiConfig
  , SafeReqSt
  , SafeReq
  , SafeReqM (..)
  ) where

import           Control.Lens
import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8              as B
import           Data.Proxy
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as E
import qualified Network.HTTP.Client                as C
import           Network.HTTP.Req

import           Network.HTTP.ApiMaker.SessionState


-- | Class definition for a 'Request'. Every request should implement this, the rest is then handled by the library. See
--  'mkReq' to create a request, the functions 'mkReqM' and 'mkSessReqM' to build a 'SafeReqM' monad that shares the
--  same state, session and configuration, and finally 'runReqM', 'runSessReqM', 'runReqWithParamsM' and
--  'runSessReqWithParamsM' to run the monad.
class (HttpMethod (Method r), HttpBody (Body r), HttpResponse (Response r), HttpBodyAllowed (AllowsBody (Method r)) (ProvidesBody (Body r))) =>
      Request cfg r
  where
  type Method r :: *
  type Body r :: *
  type Response r :: *
  type Output r :: *
  type Protocol r :: *
  method   :: cfg -> r -> Method r
  url      :: cfg -> r -> Url 'Https
  body     :: cfg -> r -> Body r
  response :: cfg -> r -> Proxy (Response r)
  option   :: cfg -> r -> Option 'Https
  process  :: (MonadHttp m, SessionState st) => cfg -> r -> Response r -> StateT st m (Output r)


-- Type safe request

data Config cfg = Config
  { httpConfig           :: HttpConfig
  , apiDefaultParameters :: [Option 'Https]
  , apiConfig            :: cfg
  }

type SafeReqSt sessionState cfg a = StateT sessionState (SafeReqM cfg) a
type SafeReq cfg a = SafeReqSt Session cfg a


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
