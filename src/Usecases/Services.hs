module Usecases.Services where

import RIO

import qualified Interfaces.DTO as IN
import qualified Interfaces.DAO as IN
import qualified Usecases.Helpers as UC
import qualified Usecases.Interactors as UC

type MakeOrder m = Monad m => IN.MakeOrderDTO -> m (Either Err ())
type SendToPipe m = Monad m => IN.NewOrderDTO -> m (Either Err ())


type NewOrdersPipe = TQueue UC.NewOrderDTO


EventPipeProcessor :: UC.Logger m => NewOrdersPipe -> IO ()
EventPipeProcessor = do
    processNewOrder


data Err = ErrTech deriving (Show, Eq)

data OrdersService m = OrdersService
    {
        _makeOrder ::  MakeOrder m
        ,_sendToPipe :: SendToPipe m
    }
 

makeOrder :: (UC.Logger m, MonadIO m) => UC.NewOrdersPipe -> IN.ProductPricesDAO m -> UC.EnrichOrderItemsDataWithPrices -> MakeOrder m
makeOrder ordersPipe productPricesDAO enrichOrderItemsDataWithPrices makeOrderData = do
    let productIDs = map IN.mOrItProductId (IN.mOrOrderItems makeOrderData)
    eitherProductsPricesMap <- IN._getMap productPricesDAO productIDs
    case eitherProductsPricesMap of
        Left err -> do
            pure $ Left ErrTech
        Right productsPricesMap -> do
            let orderItemsDTOs = enrichOrderItemsDataWithPrices productsPricesMap (IN.mOrOrderItems makeOrderData)
            let newOrderDTO = IN.NewOrderDTO (IN.mOrUserId makeOrderData) orderItemsDTOs
            UC.logDebug $ newOrderDTO
            atomically $ writeTqueue ordersPipe newOrderDTO
