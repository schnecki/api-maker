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
module Network.HTTP.ApiMaker.HeaderContent
  ( headerContentTypeJson
  , headerContentTypeMultipart
  , headerContentDispositionFile
  ) where

import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import           Network.HTTP.Req


-- | Option for specifying `application/json`.
headerContentTypeJson :: Option scheme
headerContentTypeJson = header "content-type" "application/json"

-- | Option for specifying `multipart/form-data`.
headerContentTypeMultipart :: Option scheme
headerContentTypeMultipart = header "content-type" "multipart/form-data"

-- | Option for specifying a file.
headerContentDispositionFile :: T.Text -> Option scheme
headerContentDispositionFile filename = header "Content-Disposition" (E.encodeUtf8 $ T.concat ["attachment; filename=\"", filename, "\""])


