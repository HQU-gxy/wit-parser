{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html
-- https://limperg.de/ghc-extensions/
-- https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/#any-flavor-you-like
-- https://blog.ocharles.org.uk/pages/2014-12-01-24-days-of-ghc-extensions.html
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Codec.Binary.UTF8.String (decode, encode)
import Control.Exception (throw, try)
import Control.Logger.Simple
import Data.Binary.Get
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.List (intercalate)
import Data.Maybe (catMaybes, fromJust)
import Data.Text qualified as T
import Flow
import Network.MQTT.Client (MQTTConfig (_msgCB))
import Network.MQTT.Client qualified as MC
import Network.MQTT.Topic (Topic (unTopic), mkFilter, mkTopic)
import Network.URI (parseURI)
import Prelude hiding (log)
import Control.Monad (unless)
import Control.Monad.Trans.Except
import Data.Word
import Data.Binary.Get

accCoef :: Double
accCoef = 1 / 32768 * 16

gyroCoef :: Double
gyroCoef = 1 / 32768 * 2000

angleCoef :: Double
angleCoef = 1 / 32768 * 180

-- https://wit-motion.yuque.com/wumwnr/docs/gpare3
data GyroData = GyroData
  { -- X, Y, Z linear accerleration
    -- with unit of g
    acc :: (Double, Double, Double)
  , -- roll, pitch, yaw
    -- with unit of degree
    gyro :: (Double, Double, Double)
  , -- angle accerleration
    -- with unit of degree/s
    angleAcc :: (Double, Double, Double)
  }
  deriving (Show, Eq)

magneticCoef :: Int
magneticCoef = 1

newtype MagneticData = MagneticData
  { -- X, Y, Z magnetic field
    -- with unit of mG (milligauss)
    magnetic :: (Int, Int, Int)
  }
  deriving (Show, Eq)

quaternionCoef :: Double
quaternionCoef = 1 / 32768

newtype QuaternionData = QuaternionData
  { -- W, X, Y, Z quaternion
    -- with unit of 1
    quaternion :: (Double, Double, Double, Double)
  }
  deriving (Show, Eq)

tempCoef :: Double
tempCoef = 1 / 100
newtype TemperatureData = TemperatureData
  { -- temperature
    -- with unit of Celsius
    temperature :: Double
  }
  deriving (Show, Eq)

data WitData = Gyro GyroData | Magnetic MagneticData | Quaternion QuaternionData | Temperature TemperatureData deriving (Show, Eq)

data ParseException = BadHeader | BadFlag | BadLength deriving (Show, Eq, Enum)

witHeader :: Word8
witHeader = 0x55

-- https://hackage.haskell.org/package/parsec
-- https://wiki.haskell.org/Dealing_with_binary_data
-- https://www.haskellforall.com/2021/05/the-trick-to-avoid-deeply-nested-error.html
decodeWitData :: Get (Either ParseException WitData)
decodeWitData = do
  header <- getWord8
  flag <- getWord8
  accX <- getWord16le
  accY <- getWord16le
  accZ <- getWord16le
  angleX <- getWord16le
  angleY <- getWord16le
  angleZ <- getWord16le
  roll <- getWord16le
  pitch <- getWord16le
  yaw <- getWord16le

  return $ Right $ Gyro GyroData
    { acc = (fromIntegral accX * accCoef, fromIntegral accY * accCoef, fromIntegral accZ * accCoef)
    , gyro = (fromIntegral angleX * gyroCoef, fromIntegral angleY * gyroCoef, fromIntegral angleZ * gyroCoef)
    , angleAcc = (fromIntegral roll * angleCoef, fromIntegral pitch * angleCoef, fromIntegral yaw * angleCoef)
    }

-- https://zenn.dev/tobi462/articles/4ae7658d126054
-- https://stackoverflow.com/questions/15441956/how-do-i-make-a-do-block-return-early
-- https://www.haskellforall.com/2012/07/breaking-from-loop.html
-- https://stackoverflow.com/questions/69327798/how-to-use-exceptt-to-replace-lots-of-io-either-a-b
publishWithString :: MC.MQTTClient -> String -> String -> Bool -> IO (Either MC.MQTTException ())
publishWithString c t p = do
  case mkTopic $ T.pack t of
    Nothing -> throw $ MC.MQTTException $ "Invalid topic: " <> t
    Just topic -> do
      let payload = BL.pack $ encode p
      try . MC.publish c topic payload

handleMessage :: MC.MQTTClient -> Topic -> BL.ByteString -> [MC.Property] -> IO ()
handleMessage c t p _ = do
  let topic = unTopic t
  let payload = BL.unpack p |> decode |> T.pack
  logInfo $ "topic=" <> topic <> ", payload=" <> payload

main :: IO ()
main = withGlobalLogging (LogConfig Nothing True)
  $ do
    let url = fromJust $ parseURI "mqtt://weihua-iot.cn:1883"
    -- SimpleCallback (MQTTClient -> Topic -> BL.ByteString -> [Property] -> IO ())
    let cb = MC.SimpleCallback handleMessage
    let sub_topics = ["/wit/+/data", "/test/#"]
    let subs = fmap mkFilter sub_topics |> catMaybes |> fmap (,MC.subOptions)
    -- mqttConfig is a default configuration for MQTT client
    -- https://github.com/dustin/mqtt-hs/blob/6b7c0ef075159fbd836a04ebcc8565419aa4638c/src/Network/MQTT/Client.hs#L148-L162
    mc <- MC.connectURI MC.mqttConfig{_msgCB = cb} url
    _ <- MC.subscribe mc subs []
    logInfo $ "suBcribe to: " <> T.intercalate ", " sub_topics
    MC.waitForClient mc
