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

import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Kind                          (Type)
import           Data.Proxy
import           Network.HTTP.Req

import           Network.HTTP.ApiMaker.SessionState


-- | Class definition for a 'Request'. Every request should implement this, the rest is then handled by the library. See 'Network.HTTP.ApiMaker.Ops.mkReq' to create a request, the functions
--  'Network.HTTP.ApiMaker.Ops.mkReqM' and 'Network.HTTP.ApiMaker.Ops.runRequests' to build a 'SafeReqM' monad that shares the same state, session and configuration, and finally
--  'Network.HTTP.ApiMaker.Ops.runReqM', 'Network.HTTP.ApiMaker.Ops.runSessReqM', 'Network.HTTP.ApiMaker.Ops.runReqWithParamsM' and 'Network.HTTP.ApiMaker.Ops.runSessReqWithParamsM' to run the monad.
class (HttpMethod (Method r), HttpBody (Body r), HttpResponse (Response r), HttpBodyAllowed (AllowsBody (Method r)) (ProvidesBody (Body r))) =>
      Request cfg r
  where
  type Method r :: Type
  type Body r :: Type
  type Response r :: Type
  type Output r :: Type
  -- type Protocol r :: Scheme
  method   :: cfg -> r -> Method r
  url      :: cfg -> r -> Url 'Https -- (Protocol r)
  body     :: cfg -> r -> Body r
  response :: cfg -> r -> Proxy (Response r)
  option   :: cfg -> r -> Option 'Https -- (Protocol r)
  process  :: (MonadHttp m, SessionState st) => cfg -> r -> Response r -> StateT st m (Output r)


-- Type safe request

-- | Configuration that is passed from request to request to hold the session and default https header options. It also holds a user defined configuration.
data Config cfg = Config
  { httpConfig           :: HttpConfig
  , apiDefaultParameters :: [Option 'Https]
  , apiConfig            :: cfg
  }

type SafeReqSt sessionState cfg a = StateT sessionState (SafeReqM cfg) a
type SafeReq cfg a = SafeReqSt Session cfg a

-- | Safe request, e.g. all errors are caught and tured into exceptions.
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

-- | Safely run the request monad.
runSafeReqM ::
     MonadIO m
  => Config cfg                 -- ^ Config including 'HttpConfig' to use
  -> SafeReqM cfg a             -- ^ Computation to run
  -> m (Either HttpException a)
runSafeReqM config (SafeReqM m) = liftIO (runReaderT (runExceptT m) config)


