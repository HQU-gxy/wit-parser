module Main where

import Codec.Binary.UTF8.String (encode)
import Control.Exception (throw, try)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (isJust)
import Data.Text qualified
import Network.MQTT.Client qualified as MC
import Network.MQTT.Topic (mkTopic)
import Network.URI (parseURI)
import Prelude

-- https://zenn.dev/tobi462/articles/4ae7658d126054
-- https://stackoverflow.com/questions/15441956/how-do-i-make-a-do-block-return-early
-- https://www.haskellforall.com/2012/07/breaking-from-loop.html
-- https://stackoverflow.com/questions/69327798/how-to-use-exceptt-to-replace-lots-of-io-either-a-b
publishWithString :: MC.MQTTClient -> String -> String -> Bool -> IO (Either MC.MQTTException ())
publishWithString c t p = do
  case mkTopic $ Data.Text.pack t of
    Nothing -> throw $ MC.MQTTException $ "Invalid topic: " <> t
    Just topic -> do
      let payload = LBS.pack $ encode p
      try . MC.publish c topic payload

main :: IO ()
main = do
  let Just url = parseURI "mqtt://weihua-iot.cn:1883"
  mc <- MC.connectURI MC.mqttConfig url
  result <- publishWithString mc "test" "hello" False
  case result of
    Left e -> print e
    Right _ -> print "ok"
