module Main where

import RIO
import qualified RIO.Map as Map
import System.IO
import qualified Network.Wai.Handler.Warp as Warp
import qualified Hasql.Connection as Connection
import qualified Config.Config as Config
import Kafka.Producer (ProducerProperties, TopicName)
import Usecases
import qualified Adapter.Storage.InMemory.ProductPrices as InMemoUserRepo
import qualified Adapter.Storage.Hasql.Order as HasqlOrderRepo
import qualified Adapter.Storage.Kafka.Messages as KafkaRepo
import qualified Domain.Models as D
import qualified Interfaces.DAO as IN
import qualified Interfaces.Logger as IN
import qualified Interfaces.Usecases as IN
import qualified Helpers as HP
import qualified Services.Order as UC
import qualified Services.EventPipe as UC
import qualified Services.Message as UC
import Adapter.Http.Servant.Router
import Control.Exception (throw)
import Control.Exception.Lens (AsIOException(_IOException))


main :: IO ()
main = do
    writeSwaggerJSON
    --Web server config
    port <- Config.getIntFromEnv "PORT" 3000
    storageBackend <- Config.getStringFromEnv "STORAGE" "inMem"
    --DB config
    connSettings <- HP.hasqlConnectionSettings
    conn <- Connection.acquire connSettings
    conn <- case conn of
        Right a -> pure a
        Left a -> do 
            print a
            error "shit"
    --Kafka config
    kafkaSetting <- HP.kafkaConnectionSettings
    newOrderQueue <- newTQueueIO
    router <- case storageBackend of
            "inMem" -> app (usecasesBuilder newOrderQueue inMemProductPricesDAO) $ runRIO ()
            _ -> error $ "Incorrect storage:" <> storageBackend
    
    eventPipeProcessorStart (IN.TargetTopic "new_orders_topic") kafkaSetting conn newOrderQueue $ runRIO ()
    Warp.run port router

usecasesBuilder :: (IN.Logger m, MonadUnliftIO m) => UC.NewOrdersPipe -> IN.ProductPricesDAO m -> IN.Usecases m
usecasesBuilder orderQueue productPricesDAO = IN.Usecases (
                makeOrderUsecase ordersService
            )
            where
                ordersService = UC.OrdersService (
                        UC.makeOrder orderQueue productPricesDAO HP.enrichOrderItemsDataWithPrices
                    )

inMemProductPricesDAO :: (MonadUnliftIO m) =>IN.ProductPricesDAO m
inMemProductPricesDAO = IN.ProductPricesDAO (
                InMemoUserRepo.getMap inMemoryPrices
            )
            where
                inMemoryPrices = Map.fromList [
                        (D.ProductId 1, 1.0)
                        , (D.ProductId 2, 2.0)
                        , (D.ProductId 3, 3.0)
                        , (D.ProductId 4, 4.0)
                    ]

eventPipeProcessorStart :: (IN.Logger m, MonadIO m) => IN.TargetTopic TopicName -> ProducerProperties -> Connection.Connection -> UC.NewOrdersPipe -> (forall a. m a -> IO a) -> IO ()
eventPipeProcessorStart targetTopic kafkaSettings conn orderQueue runner = UC.eventPipeProcessorRunner eventPipes eventPipeProcessorService runner
    where
        eventPipes = UC.EventPipes (
            orderQueue
            )
        eventPipeProcessorService = UC.EventPipeProcessor (
            UC.processNewOrder ordersDAO messageService targetTopic
            )
        messageService = UC.MessageService (
                UC.sendNewOrderMsg messagesDAO
            )
        messagesDAO = IN.MessagesDAO (
                KafkaRepo.sendNewOrderMsg $ KafkaRepo.getKafkaProducer kafkaSettings 
            )
        ordersDAO = IN.OrdersDAO (
                 HasqlOrderRepo.insertNewOrderWithTransaction conn
            )

instance IN.Logger (RIO a) where
  logDebug msg = liftIO $ print msg

instance IN.Logger IO where
  logDebug msg = print msg
