module Main where

import Codec.Binary.UTF8.String (encode)
import Control.Monad (guard)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (isJust)
import Data.Text qualified
import GHC.Stack (pushCallStack)
import Network.MQTT.Client qualified as MC
import Network.MQTT.Client qualified as Network.MQTT
import Network.MQTT.Topic (mkTopic)
import Network.URI (parseURI)
import Prelude

publishWithString :: Network.MQTT.MQTTClient -> String -> String -> Bool -> IO ()
publishWithString c t p = do
  case mkTopic $ Data.Text.pack t of
    Nothing -> error "Invalid topic"
    Just topic -> do
      let payload = LBS.pack $ encode p
      MC.publish c topic payload

main :: IO ()
main = do
  let mUrl = parseURI "mqtt://weihua-iot.cn:1883"
  guard $ isJust mUrl
  let Just url = mUrl
  mc <- MC.connectURI MC.mqttConfig url
  publishWithString mc "test" "hello" False
