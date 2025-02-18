{-# LANGUAGE ScopedTypeVariables #-}
module Services.Order where

import RIO

import qualified Interfaces.DTO as IN
import qualified Interfaces.DAO as IN
import Services.EventPipe
import qualified Helpers as HP
import qualified Interfaces.Logger as IN


newtype OrderErr = OrderErr String

instance Show OrderErr where
    show (OrderErr e) = e


type MakeOrder m = IN.MakeOrderDTO -> m (Either OrderErr ())


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
            handleError err
        Right productsPricesMap -> do
            let orderItemsDTOs = enrichOrderItemsDataWithPrices productsPricesMap (IN.mOrOrderItems makeOrderData)
            let newOrderDTO = IN.NewOrderDTO (IN.mOrUserId makeOrderData) orderItemsDTOs
            IN.logDebug "write"
            atomically $ writeTQueue ordersPipe newOrderDTO
            pure (Right ())
    where
        handleError (IN.ErrTechnical e) = pure $ Left $ OrderErr e
        handleError (IN.ErrValidation e) = pure $ Left $ OrderErr e
