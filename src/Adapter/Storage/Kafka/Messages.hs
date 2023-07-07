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
import qualified Interfaces.Logger as IN


type KafkaWriter m = IN.TargetTopic TopicName -> Maybe ByteString -> m (Maybe KafkaError)
-- Global producer properties
producerProps :: ProducerProperties
producerProps = brokersList ["localhost:9092"]
             <> sendTimeout (Timeout 10000)
             <> setCallback (deliveryCallback print)
             <> logLevel KafkaLogDebug

-- Topic to send messages to
-- targetTopic :: TopicName
-- targetTopic = "kafka-client-example-topic"

-- mkMessage :: TopicName -> Maybe ByteString -> Maybe ByteString -> ProducerRecord
-- mkMessage targetTopic k v = ProducerRecord
--                   { prTopic = targetTopic
--                   , prPartition = UnassignedPartition
--                   , prKey = k
--                   , prValue = v
--                   }

getKafkaProducer :: MonadIO m =>ProducerProperties -> IN.TargetTopic TopicName -> Maybe ByteString -> m (Maybe KafkaError)
getKafkaProducer props targetTopic v = newProducer props >>= producerCase >>= flip produceMessage producerRecord
    where
        producerCase = \case
                    Left e -> throw e
                    Right prod -> pure prod
        producerRecord = ProducerRecord
                  { prTopic = IN._unTTopic targetTopic
                  , prPartition = UnassignedPartition
                  , prKey = mempty
                  , prValue = v
                  }

sendNewOrderMsg :: (IN.Logger m, Monad m) => KafkaWriter m -> IN.TargetTopic TopicName -> IN.NewOrderMessageDTO -> m (Either IN.ErrDAO ())
sendNewOrderMsg producer targetTopic newOrderDTO = do
    -- pure . errHandler $ producer . Just . toStrict . encode $ newOrderDTO
    -- Just . toStrict . encode $ newOrderDTO >>= producer >>= errHandler >>= pure
    let bytes =  Just . toStrict . encode $ newOrderDTO
    b <- producer targetTopic bytes
    pure . errHandler $ b

    where
        errHandler =  \case
            Nothing -> Right ()
            Just e -> do 
                Left . IN.ErrTechnical $ displayException e
        -- makeMessage IN.NewOrderMessageDTO {..} = IN.NewOrderItemMessageDTO ()
