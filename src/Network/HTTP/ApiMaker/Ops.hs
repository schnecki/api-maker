{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.ApiMaker.Ops
  ( mkReq
  , mkReqM
  , mkSessReqM
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
import           Data.Monoid                 ((<>))
import qualified Network.HTTP.Client         as C
import           Network.HTTP.Req

import           Network.HTTP.ApiMaker.Class


runReqM :: (MonadIO m) => SafeReqM () a -> m (Either HttpException a)
runReqM = runSafeReqM (Config defaultHttpConfig [] ())

runReqWithParamsM :: (MonadIO m) => [Option 'Https] -> SafeReqM () a -> m (Either HttpException a)
runReqWithParamsM params = runSafeReqM (Config defaultHttpConfig params ())


runSessReqM :: (MonadIO m) => cfg -> SafeReqM cfg a -> m (Either HttpException a)
runSessReqM cfg = runSafeReqM (Config defaultHttpConfig [] cfg)

runSessReqWithParamsM :: (MonadIO m) => [Option 'Https] -> cfg -> SafeReqM cfg a -> m (Either HttpException a)
runSessReqWithParamsM params cfg = runSafeReqM (Config defaultHttpConfig params cfg)

mkReqM :: StateT Session (SafeReqM cfg) a -> SafeReqM cfg a
mkReqM = flip evalStateT (Session Nothing Nothing Nothing)

mkSessReqM :: st -> StateT st (SafeReqM cfg) a -> SafeReqM cfg a
mkSessReqM = flip evalStateT

mkReq :: (Request cfg request, SessionState st) => request -> SafeReqSt st cfg (Output request)
mkReq r = do
  session <- get
  cfg <- lift askConfig
  apiCfg <- lift askApiConfig

  let ops = option apiCfg r <> mkSessionOps session <> mconcat (apiDefaultParameters cfg)
  -- liftIO $ putStrLn $ "Running a request to " <> show (url r)
  response <- lift $ req (method apiCfg r) (url apiCfg r) (body apiCfg r) (response apiCfg r) ops
  updateSession r response
  process apiCfg r response
  where
    mkSessionOps session =
      maybe mempty cookieJar (session ^. cookieJarData) <> header "Cookie" (B.intercalate ";" (mkSessionCookie session ++ mkCsrfCookie session)) <> mkCsrfHeader session

updateSession :: (Request cfg request, SessionState st) => request -> Response request -> SafeReqSt st cfg ()
updateSession _ response =
  let cookies = C.destroyCookieJar $ responseCookieJar response
      sessData = C.cookie_value <$> find ((== "_SESSION") . C.cookie_name) cookies
   in sessionData .= sessData >> cookieJarData ?= responseCookieJar response


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
