{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Network.HTTP.ApiMaker.Ops
  ( mkReq
  , runRequests
  , runStRequests
  , runReqM
  , runReqWithParamsM
  , runSessReqM
  , runSessReqWithParamsM
  , maybeQueryParam
  ) where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8       as B
import           Data.List                   (find)
import qualified Data.Text                   as T
import           EasyLogger
import qualified Network.HTTP.Client         as C
import           Network.HTTP.Req
import           Web.HttpApiData

import           Network.HTTP.ApiMaker.Class


-- | Prepare to run requests.
runReqM :: (MonadIO m) => SafeReqM () a -> m (Either SafeException a)
runReqM = runSafeReqM (Config defaultHttpConfig [] ())

-- | Prepare to run requests with addional header options.
runReqWithParamsM :: (MonadIO m) => [Option 'Https] -> SafeReqM () a -> m (Either SafeException a)
runReqWithParamsM params = runSafeReqM (Config defaultHttpConfig params ())

-- | Prepare to run request with config.
runSessReqM :: (MonadIO m) => cfg -> SafeReqM cfg a -> m (Either SafeException a)
runSessReqM cfg = runSafeReqM (Config defaultHttpConfig [] cfg)

-- | Prepare to run request with config and additional header options.
runSessReqWithParamsM :: (MonadIO m) => [Option 'Https] -> cfg -> SafeReqM cfg a -> m (Either SafeException a)
runSessReqWithParamsM params cfg = runSafeReqM (Config defaultHttpConfig params cfg)


-- | Run a normal session based request state monad.
runRequests :: StateT Session (SafeReqM cfg) a -> SafeReqM cfg a
runRequests = flip evalStateT (Session Nothing Nothing Nothing)


-- | Run a user defined session request state monad.
runStRequests :: st -> StateT st (SafeReqM cfg) a -> SafeReqM cfg a
runStRequests = flip evalStateT


-- | Call a single request. See 'runRequest' and 'runStRequests' to build and execute a set of requests that share the
-- same state, session and configuration.
mkReq :: (Request cfg request, SessionState st) => request -> SafeReqSt st cfg (Output request)
mkReq r = do
  session <- get
  cfg <- lift askConfig
  apiCfg <- lift askApiConfig
  opsReq <- liftIO $ option apiCfg r
  let ops = opsReq <> mkSessionOps session <> mconcat (apiDefaultParameters cfg)
  let logging request' = do
        request <- liftIO $ requestModifier apiCfg r request'
        liftIO $ $(logInfo) $ "API Request: " <> T.pack (show request)
        return request
  resp <- lift $ reqCb (method apiCfg r) (url apiCfg r) (body apiCfg r) (response apiCfg r) ops logging
  updateSession r resp
  process apiCfg r resp
  where
    mkSessionOps session =
      maybe mempty cookieJar (session ^. cookieJarData) <> header "Cookie" (B.intercalate ";" (mkSessionCookie session ++ mkCsrfCookie session)) <> mkCsrfHeader session


updateSession :: (Request cfg request, SessionState st) => request -> Response request -> SafeReqSt st cfg ()
updateSession _ resp =
  let cookies = C.destroyCookieJar $ responseCookieJar resp
      sessData = C.cookie_value <$> find ((== "_SESSION") . C.cookie_name) cookies
   in sessionData .= sessData >> cookieJarData ?= responseCookieJar resp


-- Note: Server does not check/receive cookie!
mkSessionCookie :: (SessionState st) => st -> [B.ByteString]
mkSessionCookie st =
  case st ^. sessionData of
    Nothing   -> mempty
    Just sess -> ["_SESSION=" <> sess]

mkCsrfCookie :: (SessionState st) => st -> [B.ByteString]
mkCsrfCookie st =
  case st ^. csrfToken of
    Nothing   -> mempty
    Just csrf -> ["XSRF-TOKEN=" <> csrf]

mkCsrfHeader :: (SessionState st) => st -> Option scheme
mkCsrfHeader st =
  case st ^. csrfToken of
    Nothing   -> mempty
    Just csrf -> header "X-XSRF-TOKEN" csrf


-- | Create a request parameter only if the value is @Just@. For @Nothing@ the parameter is not present at all in the request.
maybeQueryParam :: (Monoid param, QueryParam param, ToHttpApiData a) => T.Text -> Maybe a -> param
maybeQueryParam _ Nothing  = mempty
maybeQueryParam n (Just x) = n =: x


