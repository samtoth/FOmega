{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fdefer-typed-holes #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import CoreDraw
import Data.ByteString as BS
import Data.ByteString.Lazy as LB
import Data.String (IsString)
import Data.Text as T
import Data.Text.Encoding as T
import Data.Text.Lazy as LT
import Data.Text.Lazy.Encoding as LT
import GHC.Generics
import GHC.TypeLits
import GhcDriver
import Network.HTTP.Media ((//))
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Servant

type CoreDrawApi =
  "core" :> ReqBody '[PlainText] HsSource :> Post '[Latex] CoreResult

newtype HsSource = MkSource T.Text
  deriving (IsString, MimeUnrender PlainText)

newtype CoreResult = MkCoreResult T.Text
  deriving (IsString)

data Latex

instance Accept Latex where
  contentType _ = "application" // "x-tex"

instance MimeRender Latex CoreResult where
  mimeRender _ (MkCoreResult text) = LB.fromChunks . return . T.encodeUtf8 $ text

coreServer :: Server CoreDrawApi
coreServer = coreHandler
  where
    coreHandler :: HsSource -> Handler CoreResult
    coreHandler (MkSource src) = do
      res <- liftIO $ compileSource (LT.fromStrict src)
      case res of
        Left err -> throwError $ err400 {errBody = LB.pack . read $ ("Error compiling source to core: " <> err)}
        Right cMod -> do
          pure . MkCoreResult . drawModule $ cMod

coreApi :: Proxy CoreDrawApi
coreApi = Proxy

myApp :: Application
myApp = serve coreApi coreServer

main :: IO ()
main =
  print "running on localhost:3030/"
    >> run 3030 (simpleCors $ logStdoutDev myApp)
