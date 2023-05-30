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


type NewOrdersPipe = TQueue IN.NewOrderDTO
type SuccessOrdersPipe = TQueue IN.NewOrderDTO


data EventPipeProcessor = EventPipeProcessor {
    _newOrderProcessor ::  NewOrdersPipe -> IO ()
    -- ,_successOrderProcessor :: SuccessOrdersPipe -> IO ()
}

data EventPipes = EventPipes {
    _newOrdersPipe :: NewOrdersPipe
    -- ,_successOrdersPipe :: SuccessOrdersPipe
}


processNewOrder :: (IN.Logger m, MonadUnliftIO m) => IN.OrdersDAO m -> MessageService m -> NewOrdersPipe -> m ()
processNewOrder ordersDAO messageService newOrdersPipe = do
    newOrder <- atomically $ readTQueue newOrdersPipe
    err <- runExceptT $ do
        order <- ExceptT $ IN._createOrder ordersDAO newOrder
        ExceptT $ _sendNewOrderMsg messageService (makeMassage newOrder (D.orderId order))
    either IN.logDebug IN.logDebug err
    where 
        makeMassage newOrder orderId = IN.NewOrderMessageDTO (IN.nOrUserId newOrder) 
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

-- processNewOrder :: NewOrdersPipe -> IO ()
-- processNewOrder newOrdersPipe = do
--     print "event new"
--     order <- atomically $ handle newOrdersPipe
--     print order
--     print "event new2"
--     where
--         handle (queue::NewOrdersPipe) = do
--             let order = tryReadTQueue queue
--             fromMaybe <$> retry <*> order

eventPipeProcessorRunner :: EventPipes -> EventPipeProcessor -> IO ()
eventPipeProcessorRunner eventPipes processor = do
    -- atomically $ do
    --     _newOrderProcessor processor (_newOrdersPipe eventPipes) `orElse` _successOrderProcessor processor (_successOrdersPipe eventPipes)
    void $ forkIO $ forever $ _newOrderProcessor processor (_newOrdersPipe eventPipes)


-- data EventPipeProcessor m = EventPipeProcessor {
--     _newOrderProcessor ::  NewOrdersPipe -> m ()
--     ,_successOrderProcessor :: SuccessOrdersPipe -> m ()
-- }

-- data EventPipes = EventPipes {
--     _newOrdersPipe :: NewOrdersPipe
--     ,_successOrdersPipe :: SuccessOrdersPipe
-- }


-- eventPipeProcessor :: (UC.Logger m, MonadIO m) => EventPipes -> EventPipeProcessor m -> m ()
-- eventPipeProcessor eventPipes processor = do
--     -- atomically $ do
--     --     _newOrderProcessor processor (_newOrdersPipe eventPipes) `orElse` _successOrderProcessor processor (_successOrdersPipe eventPipes)
--     void $ forkIO $ forever $ _newOrderProcessor processor (_newOrdersPipe eventPipes)
