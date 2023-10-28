{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html
-- https://limperg.de/ghc-extensions/
-- https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/#any-flavor-you-like
-- https://blog.ocharles.org.uk/pages/2014-12-01-24-days-of-ghc-extensions.html
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Codec.Binary.UTF8.String (decode, encode)
import Control.Exception (throw, try)
import Control.Logger.Simple
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromJust)
import Data.Text qualified as T
import Flow
import Network.MQTT.Client (MQTTConfig (_msgCB))
import Network.MQTT.Client qualified as MC
import Network.MQTT.Topic (Topic (unTopic), mkFilter, mkTopic)
import Network.URI (parseURI)
import Prelude hiding (log)

-- https://zenn.dev/tobi462/articles/4ae7658d126054
-- https://stackoverflow.com/questions/15441956/how-do-i-make-a-do-block-return-early
-- https://www.haskellforall.com/2012/07/breaking-from-loop.html
-- https://stackoverflow.com/questions/69327798/how-to-use-exceptt-to-replace-lots-of-io-either-a-b
publishWithString :: MC.MQTTClient -> String -> String -> Bool -> IO (Either MC.MQTTException ())
publishWithString c t p = do
  case mkTopic $ T.pack t of
    Nothing -> throw $ MC.MQTTException $ "Invalid topic: " <> t
    Just topic -> do
      let payload = LBS.pack $ encode p
      try . MC.publish c topic payload

main :: IO ()
main = withGlobalLogging (LogConfig Nothing True) $
  do
    let url = fromJust $ parseURI "mqtt://weihua-iot.cn:1883"
    -- SimpleCallback (MQTTClient -> Topic -> BL.ByteString -> [Property] -> IO ())
    let cb = MC.SimpleCallback $ \_ t p _ -> logInfo $ "topic=" <> unTopic t <> ", payload=" <> (LBS.unpack p |> decode |> T.pack)
    let sub_topics = ["/wit/+/data", "/test/#"]
    let subs = fmap mkFilter sub_topics |> catMaybes |> fmap (,MC.subOptions)
    -- mqttConfig is a default configuration for MQTT client
    -- https://github.com/dustin/mqtt-hs/blob/6b7c0ef075159fbd836a04ebcc8565419aa4638c/src/Network/MQTT/Client.hs#L148-L162
    mc <- MC.connectURI MC.mqttConfig {_msgCB = cb} url
    _ <- MC.subscribe mc subs []
    logInfo $ "subscribe to: " <> T.intercalate ", " sub_topics
    MC.waitForClient mc
