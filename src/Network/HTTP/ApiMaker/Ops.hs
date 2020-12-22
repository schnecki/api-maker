{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.ApiMaker.Ops
  ( mkReq
  , runRequests
  , runStRequests
  , runReqM
  , runReqWithParamsM
  , runSessReqM
  , runSessReqWithParamsM
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8       as B
import           Data.List                   (find)
import qualified Network.HTTP.Client         as C
import           Network.HTTP.Req

import           Network.HTTP.ApiMaker.Class


-- | Prepare to run requests.
runReqM :: (MonadIO m) => SafeReqM () a -> m (Either HttpException a)
runReqM = runSafeReqM (Config defaultHttpConfig [] ())

-- | Prepare to run requests with addional header options.
runReqWithParamsM :: (MonadIO m) => [Option 'Https] -> SafeReqM () a -> m (Either HttpException a)
runReqWithParamsM params = runSafeReqM (Config defaultHttpConfig params ())

-- | Prepare to run request with config.
runSessReqM :: (MonadIO m) => cfg -> SafeReqM cfg a -> m (Either HttpException a)
runSessReqM cfg = runSafeReqM (Config defaultHttpConfig [] cfg)

-- | Prepare to run request with config and additional header options.
runSessReqWithParamsM :: (MonadIO m) => [Option 'Https] -> cfg -> SafeReqM cfg a -> m (Either HttpException a)
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

  let ops = option apiCfg r <> mkSessionOps session <> mconcat (apiDefaultParameters cfg)
  -- liftIO $ putStrLn $ "Running a request to " <> show (url r)
  resp <- lift $ req (method apiCfg r) (url apiCfg r) (body apiCfg r) (response apiCfg r) ops
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
