module Helpers where

import RIO
import qualified RIO.Map as Map
import qualified Interfaces.DAO as IN
import qualified Interfaces.DTO as IN
import qualified Hasql.Connection as Connection
import Config.Config as Config
import qualified RIO.Text as T
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
import System.IO (print)

type EnrichOrderItemsDataWithPrices = IN.ProductPricesMap -> [IN.MakeOrderItemDTO] -> [IN.NewOrderItemDTO]

enrichOrderItemsDataWithPrices :: EnrichOrderItemsDataWithPrices
enrichOrderItemsDataWithPrices productPricesMap =
    map f
    where
        f makeOrderItemDTO = IN.NewOrderItemDTO productId (IN.mOrItCount makeOrderItemDTO) (maybeLookup (Map.lookup productId productPricesMap))
          where
            productId = IN.mOrItProductId makeOrderItemDTO
            maybeLookup Nothing = 0
            maybeLookup (Just val) = val

hasqlConnectionSettings :: IO Connection.Settings
hasqlConnectionSettings = do
  dbHost <- Config.getStringFromEnv "POSTGRES_HOST" "localhost"
  dbPort <- Config.getIntFromEnv "POSTGRES_PORT" 5432
  dbUser <- Config.getStringFromEnv "POSTGRES_USER" "user"
  dbPassword <- Config.getStringFromEnv "POSTGRES_PASSWORD" "password"
  dbName <- Config.getStringFromEnv "POSTGRES_DB" "mydb"
  pure $ Connection.settings (bString dbHost) (fromIntegral dbPort) (bString dbUser) (bString dbPassword) (bString dbName)
  where
    bString = T.encodeUtf8 . T.pack

producerProps :: ProducerProperties
producerProps = brokersList ["localhost:9093"]
             <> sendTimeout (Timeout 10000)
             <> setCallback (deliveryCallback print)
             <> logLevel KafkaLogDebug

kafkaConnectionSettings :: IO ProducerProperties
kafkaConnectionSettings = pure producerProps
