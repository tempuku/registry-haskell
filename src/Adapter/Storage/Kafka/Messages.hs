{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Adapter.Storage.Kafka.Messages where

import Kafka.Producer.Types
    ( KafkaProducer,
      ProducePartition(UnassignedPartition),
      ProducerRecord(..) )
import Kafka.Producer
    ( KafkaLogLevel(KafkaLogDebug),
      Timeout(Timeout),
      TopicName,
      closeProducer,
      newProducer,
      produceMessage,
      deliveryCallback,
      brokersList,
      logLevel,
      sendTimeout,
      setCallback,
      ProducerProperties, KafkaError )
import RIO
import System.IO ( print, putStrLn, )
import RIO.ByteString (getLine)
import Control.Exception (throw)
import qualified Interfaces.DAO as IN
import qualified Interfaces.DTO as IN
import Data.Aeson (encode)
import RIO.ByteString.Lazy (toStrict)


type KafkaWriter m = Maybe ByteString -> m (Maybe KafkaError)
-- Global producer properties
producerProps :: ProducerProperties
producerProps = brokersList ["localhost:9092"]
             <> sendTimeout (Timeout 10000)
             <> setCallback (deliveryCallback print)
             <> logLevel KafkaLogDebug

-- Topic to send messages to
targetTopic :: TopicName
targetTopic = "kafka-client-example-topic"

mkMessage :: Maybe ByteString -> Maybe ByteString -> ProducerRecord
mkMessage k v = ProducerRecord
                  { prTopic = targetTopic
                  , prPartition = UnassignedPartition
                  , prKey = k
                  , prValue = v
                  }

-- getKafkaProducer :: ProducerProperties -> IO KafkaProducer
-- getKafkaProducer props = newProducer props >>= \case
--                     Left e -> throw e
--                     Right prod -> pure prod

getKafkaProducer :: ProducerProperties -> Maybe ByteString -> IO (Maybe KafkaError)
getKafkaProducer props v = newProducer props >>= producerCase >>= flip produceMessage producerRecord
    where
        producerCase = \case
                    Left e -> throw e
                    Right prod -> pure prod
        producerRecord = ProducerRecord
                  { prTopic = targetTopic
                  , prPartition = UnassignedPartition
                  , prKey = mempty
                  , prValue = v
                  }

-- Run an example
-- runProducerExample :: IO ()
-- runProducerExample =
--     bracket mkProducer clProducer runHandler >>= print
--     where
--       mkProducer = newProducer producerProps
--       clProducer (Left _)     = return ()
--       clProducer (Right prod) = closeProducer prod
--       runHandler (Left err)   = return $ Left err
--       runHandler (Right prod) = sendMessages prod

-- sendMessages :: KafkaProducer -> IO (Either KafkaError ())
-- sendMessages prod = do
--   putStrLn "Producer is ready, send your messages!"
--   msg1 <- getLine

--   err1 <- produceMessage prod (mkMessage (Just "zero") (Just msg1))
--   forM_ err1 print

--   putStrLn "One more time!"
--   msg2 <- getLine

--   err2 <- produceMessage prod (mkMessage (Just "key") (Just msg2))
--   forM_ err2 print

--   putStrLn "And the last one..."
--   msg3 <- getLine
--   err3 <- produceMessage prod (mkMessage (Just "key3") (Just msg3))
--   -- forM_ errs (print . snd)

--   putStrLn "Thank you."
--   return $ Right ()

sendNewOrderMsg :: Monad m => KafkaWriter m -> IN.NewOrderMessageDTO -> m (Either IN.ErrDAO ())
sendNewOrderMsg producer newOrderDTO = do
    -- pure . errHandler $ producer . Just . toStrict . encode $ newOrderDTO
    -- Just . toStrict . encode $ newOrderDTO >>= producer >>= errHandler >>= pure
    let bytes =  Just . toStrict . encode $ newOrderDTO
    b <- producer bytes
    pure . errHandler $ b
    where
        errHandler =  \case
            Nothing -> Right ()
            Just e -> Left . IN.ErrTechnical $ displayException e
        -- makeMessage IN.NewOrderMessageDTO {..} = IN.NewOrderItemMessageDTO ()
