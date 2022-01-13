{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures -fdefer-typed-holes #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import CoreDraw
import Data.Aeson
import Data.Aeson.TH
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
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger
import Servant

data HsSource = MkSource {extensions :: T.Text, source :: T.Text}
  deriving (Generic, Show)

instance FromJSON HsSource

toSource :: HsSource -> T.Text
toSource MkSource {source, extensions} = case T.unpack extensions of
  [] -> source
  _ -> "{-# LANGUAGE " <> extensions <> " #-}\n" <> source

data CoreDrawResult
  = Success ProcessedModule
  | Error T.Text
  deriving (Generic, Show)

instance ToJSON CoreDrawResult

type CoreDrawApi =
  "core" :> ReqBody '[JSON] HsSource :> Post '[JSON] CoreDrawResult

coreServer :: Server CoreDrawApi
coreServer = coreHandler
  where
    coreHandler :: HsSource -> Handler CoreDrawResult
    coreHandler src = do
      res <- liftIO $ compileSource (LT.fromStrict . toSource $ src)
      case res of
        Left err -> pure . Main.Error $ ("GHC Error: \n\t" <> T.pack err)
        Right cMod -> pure . Main.Success . drawModule $ cMod

coreApi :: Proxy CoreDrawApi
coreApi = Proxy

myApp :: Application
myApp = serve coreApi coreServer

corsWithContentType :: Middleware
corsWithContentType = cors (const $ Just policy)
  where
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"]
        }

main :: IO ()
main =
  print "running on localhost:3030/"
    >> run 3030 (corsWithContentType . logStdoutDev $ myApp)
