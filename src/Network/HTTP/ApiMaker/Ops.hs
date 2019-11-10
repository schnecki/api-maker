{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.HTTP.ApiMaker.Ops
  ( mkReq
  , SafeReq
  , runReqM
  , runReqWithParamsM
  , runCfgReqM
  , runCfgReqWithParamsM
  ) where

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


runCfgReqM :: (MonadIO m) => cfg -> SafeReqM cfg a -> m (Either HttpException a)
runCfgReqM cfg = runSafeReqM (Config defaultHttpConfig [] cfg)

runCfgReqWithParamsM :: (MonadIO m) => [Option 'Https] -> cfg -> SafeReqM cfg a -> m (Either HttpException a)
runCfgReqWithParamsM params cfg = runSafeReqM (Config defaultHttpConfig params cfg)


type SafeReq cfg a = StateT Session (SafeReqM cfg) a

mkReq :: (Request cfg request) => request -> StateT Session (SafeReqM cfg) (Output request)
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
      maybe mempty cookieJar (cookieJarData session) <> header "Cookie" (B.intercalate ";" (mkSessionCookie session ++ mkCsrfCookie session)) <> mkCsrfHeader session

updateSession :: (Request cfg request) => request -> Response request -> StateT Session (SafeReqM cfg) ()
updateSession _ response =
  let cookies = C.destroyCookieJar $ responseCookieJar response
      sessData = C.cookie_value <$> find ((== "_SESSION") . C.cookie_name) cookies
   in modify (\s -> s {sessionData = sessData, cookieJarData = Just $ responseCookieJar response})

-- Note: Server does not check/receive cookie!
mkSessionCookie :: Session -> [B.ByteString]
mkSessionCookie (Session _ (Just sess) _) = ["_SESSION=" <> sess]
mkSessionCookie _                         = mempty

mkCsrfCookie :: Session -> [B.ByteString]
mkCsrfCookie (Session (Just csrf) _ _) = ["XSRF-TOKEN=" <> csrf]
mkCsrfCookie _                         = mempty

mkCsrfHeader :: Session -> Option scheme
mkCsrfHeader (Session (Just csrf) _ _) = header "X-XSRF-TOKEN" csrf
mkCsrfHeader _                         = mempty

