module Main where

import Codec.Binary.UTF8.String (encode)
import Control.Exception (throw, try)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.Text qualified as Tx
import Flow
import Network.MQTT.Client (MQTTConfig (_msgCB))
import Network.MQTT.Client qualified as MC
import Network.MQTT.Topic (mkFilter, mkTopic)
import Network.URI (parseURI)
import Prelude

-- https://zenn.dev/tobi462/articles/4ae7658d126054
-- https://stackoverflow.com/questions/15441956/how-do-i-make-a-do-block-return-early
-- https://www.haskellforall.com/2012/07/breaking-from-loop.html
-- https://stackoverflow.com/questions/69327798/how-to-use-exceptt-to-replace-lots-of-io-either-a-b
publishWithString :: MC.MQTTClient -> String -> String -> Bool -> IO (Either MC.MQTTException ())
publishWithString c t p = do
  case mkTopic $ Tx.pack t of
    Nothing -> throw $ MC.MQTTException $ "Invalid topic: " <> t
    Just topic -> do
      let payload = LBS.pack $ encode p
      try . MC.publish c topic payload

main :: IO ()
main = do
  let url = fromJust $ parseURI "mqtt://weihua-iot.cn:1883"
  -- SimpleCallback (MQTTClient -> Topic -> BL.ByteString -> [Property] -> IO ())
  let cb = MC.SimpleCallback $ \_ t p _ -> print $ "topic=" <> show t <> ", payload=" <> show p
  let sub_topics = ["/wit/+/data", "/test/#"]
  let subs = fmap (mkFilter . Tx.pack) sub_topics |> catMaybes |> fmap (, MC.subOptions) 
  -- mqttConfig is a default configuration for MQTT client
  -- https://github.com/dustin/mqtt-hs/blob/6b7c0ef075159fbd836a04ebcc8565419aa4638c/src/Network/MQTT/Client.hs#L148-L162
  mc <- MC.connectURI MC.mqttConfig {_msgCB = cb} url
  _ <- MC.subscribe mc subs []
  _ <- print $ "subscribe to: " <> intercalate ", " sub_topics
  MC.waitForClient mc
