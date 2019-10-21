{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module ApiMaker.Ops
  ( mkReq
  , runReqM
  , runReqWithParamsM
  , runCfgReqM
  , runCfgReqWithParamsM
  ) where

import           Control.Monad.Except
import           Control.Monad.Trans.State
import qualified Data.ByteString.Char8     as B
import           Data.List                 (find)
import           Data.Monoid               ((<>))
import qualified Network.HTTP.Client       as C
import           Network.HTTP.Req

import           ApiMaker.Class


runReqM :: (MonadIO m) => ReqSafe () a -> m (Either HttpException a)
runReqM = runReqSafe (Config defaultHttpConfig [] ())

runReqWithParamsM :: (MonadIO m) => [Option 'Https] -> ReqSafe () a -> m (Either HttpException a)
runReqWithParamsM params = runReqSafe (Config defaultHttpConfig params ())


runCfgReqM :: (MonadIO m) => cfg -> ReqSafe cfg a -> m (Either HttpException a)
runCfgReqM cfg = runReqSafe (Config defaultHttpConfig [] cfg)

runCfgReqWithParamsM :: (MonadIO m) => [Option 'Https] -> cfg -> ReqSafe cfg a -> m (Either HttpException a)
runCfgReqWithParamsM params cfg = runReqSafe (Config defaultHttpConfig params cfg)


mkReq :: (Request cfg request) => request -> StateT Session (ReqSafe cfg) (Output request)
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

updateSession :: (Request cfg request) => request -> Response request -> StateT Session (ReqSafe cfg) ()
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

