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
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Data.Aeson
import Data.Aeson.TH
import Network.Wai (Middleware)

data HsSource = MkSource {extensions :: T.Text, source :: T.Text}

deriveJSON defaultOptions ''HsSource

toSource :: HsSource -> T.Text
toSource MkSource {source, extensions} = case T.unpack extensions of
                                          [] -> source
                                          _  ->  "{-# LANGUAGE " <> extensions <> " #-}\n" <> source 

newtype CoreResult = MkCoreResult T.Text
  deriving (IsString)

data Latex

instance Accept Latex where
  contentType _ = "application" // "x-tex"

instance MimeRender Latex CoreResult where
  mimeRender _ (MkCoreResult text) = LB.fromChunks . return . T.encodeUtf8 $ text

type CoreDrawApi =
  "core" :> ReqBody '[JSON] HsSource :> Post '[Latex] CoreResult

coreServer :: Server CoreDrawApi
coreServer = coreHandler
  where
    coreHandler :: HsSource -> Handler CoreResult
    coreHandler src = do
      res <- liftIO $ compileSource (LT.fromStrict.toSource $  src)
      case res of
        Left err -> throwError $ err400 {errBody = LB.pack . read $ ("Error compiling source to core: " <> err)}
        Right cMod -> do
          pure . MkCoreResult . drawModule $ cMod

coreApi :: Proxy CoreDrawApi
coreApi = Proxy

myApp :: Application
myApp = serve coreApi coreServer

corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
    where
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"] }

main :: IO ()
main =
  print "running on localhost:3030/"
    >> run 3030 (corsWithContentType . logStdoutDev $ myApp)
