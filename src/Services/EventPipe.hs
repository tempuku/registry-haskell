{-# LANGUAGE ScopedTypeVariables #-}
module Services.EventPipe where

import RIO

import Services.Message
import qualified Domain.Models as D
import qualified Interfaces.DTO as IN
import qualified Interfaces.DAO as IN
import qualified Interfaces.Logger as IN
import Control.Concurrent (forkIO)
import Control.Monad.Except (runExceptT, ExceptT (ExceptT))
import Interfaces.DAO (TargetTopic)


type NewOrdersPipe = TQueue IN.NewOrderDTO
type SuccessOrdersPipe = TQueue IN.NewOrderDTO


data EventPipeProcessor m = EventPipeProcessor {
    _newOrderProcessor :: (MonadIO m) =>  NewOrdersPipe -> m ()
    -- ,_successOrderProcessor :: SuccessOrdersPipe -> IO ()
}

data EventPipes = EventPipes {
    _newOrdersPipe :: NewOrdersPipe
    -- ,_successOrdersPipe :: SuccessOrdersPipe
}

processNewOrder :: (IN.Logger m, MonadIO m) => IN.OrdersDAO m -> MessageService m a -> TargetTopic a -> NewOrdersPipe -> m ()
processNewOrder IN.OrdersDAO {..} messageService targetTopic newOrdersPipe = do
    newOrder <- atomically $ readTQueue newOrdersPipe
    err <- runExceptT $ do
        order <- ExceptT $ _createOrder newOrder
        ExceptT $ _sendNewOrderMsg messageService targetTopic (makeMessage newOrder (D.orderId order))
    either IN.logDebug IN.logDebug err
    pure ()
    where 
        makeMessage newOrder orderId = IN.NewOrderMessageDTO (IN.nOrUserId newOrder) 
            (
                map 
                (\orderItem -> IN.NewOrderItemMessageDTO 
                    (IN.nOrItProductId orderItem)  
                    (IN.nOrItCount orderItem) 
                    (IN.nOrItProductPrice orderItem)
                    orderId
                ) 
                (IN.nOrOrderItems newOrder)
            )

eventPipeProcessorRunner :: MonadIO m => EventPipes -> EventPipeProcessor m -> (forall a. m a -> IO a) -> IO ()
eventPipeProcessorRunner eventPipes processor runner = do
    void $ forkIO $ forever $ runner $ _newOrderProcessor processor (_newOrdersPipe eventPipes)
