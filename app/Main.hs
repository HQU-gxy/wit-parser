{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
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
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Data.Binary.Get
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (catMaybes, fromJust)
import Data.Text qualified as T
import Data.Word
import Flow
import Network.MQTT.Client (MQTTConfig (_msgCB))
import Network.MQTT.Client qualified as MC
import Network.MQTT.Topic (Topic (unTopic), mkFilter, mkTopic)
import Network.URI (parseURI)
import Prelude hiding (log)

accCoef :: Double
accCoef = 1 / 32768 * 16

gyroCoef :: Double
gyroCoef = 1 / 32768 * 2000

angleCoef :: Double
angleCoef = 1 / 32768 * 180

witHeader :: Word8
witHeader = 0x55
gyroFlag :: Word8
gyroFlag = 0x61
regFlag :: Word8
regFlag = 0x71

temperatureReg :: Word8
temperatureReg = 0x40
quaternionReg :: Word8
quaternionReg = 0x51
magneticReg :: Word8
magneticReg = 0x3A


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

-- https://stackoverflow.com/questions/9722689/haskell-how-to-map-a-tuple
map4Tuple :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
map4Tuple f (a, b, c, d) = (f a, f b, f c, f d)

map3Tuple :: (a -> b) -> (a, a, a) -> (b, b, b)
map3Tuple f (a, b, c) = (f a, f b, f c)

-- https://hackage.haskell.org/package/parsec
-- https://wiki.haskell.org/Dealing_with_binary_data
-- https://www.haskellforall.com/2021/05/the-trick-to-avoid-deeply-nested-error.html
-- https://www.reddit.com/r/haskell/comments/koyv6g/syntax_for_early_return_in_donotation_ghc_plugin/
decodeData :: Get (Either ParseException WitData)
decodeData = runExceptT $ do
  header <- lift getWord8
  if header /= witHeader
    then except $ Left BadHeader
    else do
      flag <- lift getWord8
      if flag == gyroFlag
        then do
          d <- lift decodeGyroData
          lift $ pure $ Gyro d
        else
          if flag == regFlag
            then do
              reg <- lift getWord8
              _ <- lift getWord8
              if
                | reg == quaternionReg -> do
                    d <- lift decodeQuaternionData
                    lift $ pure $ Quaternion d
                | reg == magneticReg -> do
                    d <- lift decodeMagneticData
                    lift $ pure $ Magnetic d
                | reg == temperatureReg -> do
                    d <- lift decodeTemperatureData
                    lift $ pure $ Temperature d
                | otherwise -> except $ Left BadFlag
            else except $ Left BadFlag

decodeQuaternionData :: Get QuaternionData
decodeQuaternionData = do
  w <- getWord16le
  x <- getWord16le
  y <- getWord16le
  z <- getWord16le
  pure
    $ QuaternionData
      { quaternion = (w, x, y, z) |> map4Tuple ((* quaternionCoef) . fromIntegral)
      }

decodeTemperatureData :: Get TemperatureData
decodeTemperatureData = do
  temp <- getWord16le
  pure
    $ TemperatureData
      { temperature = fromIntegral temp * tempCoef
      }

decodeGyroData :: Get GyroData
decodeGyroData = do
  accX <- getWord16le
  accY <- getWord16le
  accZ <- getWord16le
  angleX <- getWord16le
  angleY <- getWord16le
  angleZ <- getWord16le
  roll <- getWord16le
  pitch <- getWord16le
  yaw <- getWord16le
  pure
    $ GyroData
      { acc = (accX, accY, accZ) |> map3Tuple ((* accCoef) . fromIntegral)
      , gyro = (angleX, angleY, angleZ) |> map3Tuple ((* gyroCoef) . fromIntegral)
      , angleAcc = (roll, pitch, yaw) |> map3Tuple ((* angleCoef) . fromIntegral)
      }

decodeMagneticData :: Get MagneticData
decodeMagneticData = do
  _ <- getWord8
  magX <- getWord16le
  magY <- getWord16le
  magZ <- getWord16le
  pure
    $ MagneticData
      { magnetic = (magX, magY, magZ) |> map3Tuple ((* magneticCoef) . fromIntegral)
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
