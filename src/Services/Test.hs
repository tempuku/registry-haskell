module Services.Test where

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