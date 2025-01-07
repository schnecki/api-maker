{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
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
  , SafeException (..)
  , throwUserException
  , catchError
  , throwError
  ) where

import           Control.Exception
import           Control.Monad.Base
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Dynamic
import           Data.Kind                          (Type)
import           Data.Proxy
import qualified Network.HTTP.Client                as C
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
  option   :: cfg -> r -> IO (Option 'Https) -- (Protocol r)
  requestModifier :: cfg -> r -> C.Request -> IO C.Request
  requestModifier _ _ = return
  process  :: (MonadHttp m, MonadError SafeException m, SessionState st) => cfg -> r -> Response r -> StateT st m (Output r)


-- Type safe request

-- | Configuration that is passed from request to request to hold the session and default https header options. It also holds a user defined configuration.
data Config cfg = Config
  { httpConfig           :: HttpConfig
  , apiDefaultParameters :: [Option 'Https]
  , apiConfig            :: cfg
  }

-- | Safe request monad with customized session state `sessionState`, Config `cfg` and result `a`.
type SafeReqSt sessionState cfg a = StateT sessionState (SafeReqM cfg) a

-- | Safe request monad with predetermined @Session@, config `cfg` and result `a`.
type SafeReq cfg a = SafeReqSt Session cfg a


data SafeException
  = ReqException HttpException
  | forall e. (Typeable e, Exception e) =>
              SafeUserException e

instance Exception SafeException
instance Show SafeException where
  show (ReqException e)      = show e
  show (SafeUserException e) = show e


-- | Safe request, e.g. all errors are caught and tured into exceptions.
newtype SafeReqM cfg a =
  SafeReqM (ExceptT SafeException (ReaderT (Config cfg) IO) a)
  deriving (Functor, Applicative, Monad, MonadIO)

askConfig :: SafeReqM cfg (Config cfg)
askConfig = SafeReqM (lift ask)

askApiConfig :: SafeReqM cfg cfg
askApiConfig = apiConfig <$> SafeReqM (lift ask)

instance MonadBase IO (SafeReqM cfg) where
  liftBase = liftIO

instance MonadHttp (SafeReqM cfg) where
  handleHttpException = SafeReqM . throwError . ReqException
  getHttpConfig = httpConfig <$> SafeReqM (lift ask)

instance MonadError SafeException (SafeReqM cfg) where
  throwError = SafeReqM . throwError . SafeUserException
  catchError c h = do
    cfg <- askConfig
    res <- runSafeReqM cfg c
    case res of
      Left ex -> h ex
      Right{} -> SafeReqM $ ExceptT $ return res


-- | Throw an Exception to the `SafeReqM` Monad.
throwUserException :: (MonadError SafeException m, Exception e) => e -> m a
throwUserException = throwError . SafeUserException


-- | Safely run the request monad.
runSafeReqM ::
     MonadIO m
  => Config cfg                 -- ^ Config including 'HttpConfig' to use
  -> SafeReqM cfg a             -- ^ Computation to run
  -> m (Either SafeException a)
runSafeReqM config (SafeReqM m) = liftIO (runReaderT (runExceptT m) config)
