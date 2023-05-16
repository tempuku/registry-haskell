{-# LANGUAGE ScopedTypeVariables #-}
module Services where

import RIO

import qualified Interfaces.DTO as IN
import qualified Interfaces.DAO as IN
import qualified Helpers as HP
import qualified Interfaces.Logger as IN
import Control.Concurrent (forkIO)
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


processNewOrder :: (IN.Logger m, MonadIO m) =>  NewOrdersPipe -> m ()
processNewOrder newOrdersPipe = do
    IN.logDebug "wait"
    -- waitForTrue newOrdersPipe
    order <- atomically $ readTQueue newOrdersPipe
    IN.logDebug "get value"
    IN.logDebug order

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


type MakeOrder m = IN.MakeOrderDTO -> m (Either Err ())


data Err = ErrTech deriving (Show, Eq)

data OrdersService m = OrdersService
    {
        _makeOrder ::  MakeOrder m
    }


makeOrder :: (IN.Logger m, MonadIO m) => NewOrdersPipe -> IN.ProductPricesDAO m -> HP.EnrichOrderItemsDataWithPrices -> MakeOrder m
makeOrder ordersPipe productPricesDAO enrichOrderItemsDataWithPrices makeOrderData = do
    let productIDList = map IN.mOrItProductId (IN.mOrOrderItems makeOrderData)
    eitherProductsPricesMap <- IN._getMap productPricesDAO productIDList
    case eitherProductsPricesMap of
        Left err -> do
            pure $ Left ErrTech
        Right productsPricesMap -> do
            let orderItemsDTOs = enrichOrderItemsDataWithPrices productsPricesMap (IN.mOrOrderItems makeOrderData)
            let newOrderDTO = IN.NewOrderDTO (IN.mOrUserId makeOrderData) orderItemsDTOs
            IN.logDebug "write"
            atomically $ writeTQueue ordersPipe newOrderDTO
            pure (Right ())
