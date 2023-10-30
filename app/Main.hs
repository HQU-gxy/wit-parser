{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
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

import Codec.Binary.UTF8.String qualified as UTF8
import Control.Exception (throw, try)
import Control.Logger.Simple
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Binary.Get
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL
import Data.Int (Int16, Int8)
import Data.Maybe (catMaybes, fromJust)
import Data.Text qualified as T
import Data.Word
import Deriving.Aeson
import Flow
import Network.MQTT.Client (MQTTConfig (_msgCB))
import Network.MQTT.Client qualified as MC
import Network.MQTT.Topic (Topic (unTopic), mkFilter, mkTopic)
import Network.MQTT.Topic qualified as MC
import Network.URI (parseURI)
import Prelude hiding (log)

accelCoef :: Double
accelCoef = (1 / 32768) * 16

angleVelCoef :: Double
angleVelCoef = 1 / 32768 * 2000

degreeCoef :: Double
degreeCoef = 1 / 32768 * 180

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
    accel :: (Double, Double, Double)
  , -- roll, pitch, yaw
    -- with unit of degree
    angleVel :: (Double, Double, Double)
  , -- angle accerleration
    -- with unit of degree/s
    angle :: (Double, Double, Double)
  }
  deriving (Generic)
  deriving
    (ToJSON)
    via CustomJSON '[FieldLabelModifier '[CamelToSnake]] GyroData

magneticCoef :: Int16
magneticCoef = 1

newtype MagneticData = MagneticData
  { -- X, Y, Z magnetic field
    -- with unit of mG (milligauss)
    magnetic :: (Int16, Int16, Int16)
  }
  deriving (Show, Eq, ToJSON)

quaternionCoef :: Double
quaternionCoef = 1 / 32768

newtype QuaternionData = QuaternionData
  { -- W, X, Y, Z quaternion
    -- with unit of 1
    quaternion :: (Double, Double, Double, Double)
  }
  deriving (Show, Eq, ToJSON)

tempCoef :: Double
tempCoef = 1 / 100
newtype TemperatureData = TemperatureData
  { -- temperature
    -- with unit of Celsius
    temperature :: Double
  }
  deriving (Show, Eq, ToJSON)

data WitData = Gyro GyroData | Magnetic MagneticData | Quaternion QuaternionData | Temperature TemperatureData
  deriving (Generic)
  deriving (ToJSON) via CustomJSON '[FieldLabelModifier '[CamelToSnake]] WitData

data ParseException = BadHeader | BadFlag | BadLength | BadRegister deriving (Show, Eq, Enum)

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
                | otherwise -> except $ Left BadRegister
            else except $ Left BadFlag

decodeQuaternionData :: Get QuaternionData
decodeQuaternionData = do
  w <- getWord16le
  x <- getWord16le
  y <- getWord16le
  z <- getWord16le
  pure
    $ QuaternionData
      { quaternion = (w, x, y, z) |> map4Tuple ((* quaternionCoef) . fromSignedWord16)
      }

decodeTemperatureData :: Get TemperatureData
decodeTemperatureData = do
  temp <- getWord16le
  pure
    $ TemperatureData
      { temperature = fromSignedWord16 temp * tempCoef
      }

-- if you use (fromIntegral::Word16 -> Double) directly
-- you would get a unsigned value
fromSignedWord16 :: Word16 -> Double
fromSignedWord16 = (fromIntegral :: Int16 -> Double) . fromIntegral

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
      { accel = (accX, accY, accZ) |> map3Tuple ((* accelCoef) . fromSignedWord16)
      , angleVel = (angleX, angleY, angleZ) |> map3Tuple ((* angleVelCoef) . fromSignedWord16)
      , angle = (roll, pitch, yaw) |> map3Tuple ((* degreeCoef) . fromSignedWord16)
      }

decodeMagneticData :: Get MagneticData
decodeMagneticData = do
  _ <- getWord8
  magX <- getWord16le
  magY <- getWord16le
  magZ <- getWord16le
  pure
    $ MagneticData
      { magnetic = (magX, magY, magZ) |> map3Tuple ((* magneticCoef) . (fromIntegral :: Word16 -> Int16))
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
      let payload = BL.pack $ UTF8.encode p
      try . MC.publish c topic payload

-- old topic -> new suffix -> new topic
fromOldTopic :: T.Text -> T.Text -> Maybe T.Text
fromOldTopic topic suffix = do
  let xs = T.splitOn "/" topic
  let prefix = xs !! 1
  if prefix /= "wit"
    then Nothing
    else do
      let id = xs !! 2
      let newTopic = T.intercalate "/" [prefix, id, suffix]
      Just newTopic

handleMessage :: MC.MQTTClient -> Topic -> BL.ByteString -> [MC.Property] -> IO ()
handleMessage c t p _ = do
  let topic = unTopic t
  -- let payload = BL.unpack p |> UTF8.decode |> T.pack
  -- logInfo $ "topic=" <> topic <> ", length=" <> T.pack (show (BL.length p))
  -- Why? how the fuck is this monad trans work?
  result <- runExceptT $ ExceptT (pure $ runGet decodeData p)
  case result of
    Left e -> logError $ "parse error: " <> T.pack (show e)
    Right d -> do
      let suffix = case d of
            Gyro _ -> "gyro"
            Magnetic _ -> "magnetic"
            Temperature _ -> "temperature"
            Quaternion _ -> "quaternion"
      case MC.mkTopic <$> fromOldTopic topic suffix of
        Just (Just newTopic) -> do
          let bl = encode d
          let s = BL.unpack bl |> UTF8.decode |> T.pack
          logInfo $ "topic=" <> unTopic newTopic <> ", payload=" <> s
          MC.publish c newTopic bl False
        _ -> logError $ "invalid topic" <> topic

main :: IO ()
main = withGlobalLogging (LogConfig Nothing True)
  $ do
    let url = fromJust $ parseURI "mqtt://weihua-iot.cn:1883"
    -- SimpleCallback (MQTTClient -> Topic -> BL.ByteString -> [Property] -> IO ())
    let cb = MC.SimpleCallback handleMessage
    let sub_topics = ["/wit/+/data"]
    let subs = fmap mkFilter sub_topics |> catMaybes |> fmap (,MC.subOptions)
    -- mqttConfig is a default configuration for MQTT client
    -- https://github.com/dustin/mqtt-hs/blob/6b7c0ef075159fbd836a04ebcc8565419aa4638c/src/Network/MQTT/Client.hs#L148-L162
    mc <- MC.connectURI MC.mqttConfig{_msgCB = cb} url
    _ <- MC.subscribe mc subs []
    logInfo $ "subscribe to: " <> T.intercalate ", " sub_topics
    MC.waitForClient mc
