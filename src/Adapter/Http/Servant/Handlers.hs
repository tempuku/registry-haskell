module Adapter.Http.Servant.Handlers where

import RIO hiding (Handler)
import Servant (err404, Handler, ServerError (errBody))

import Adapter.Http.Servant.Schemas
import Interfaces.Usecases
import qualified Interfaces.DTO as IN
import qualified Domain.Models as D
import qualified Adapter.Http.Servant.Schemas
import qualified Interfaces.Logger as IN
import Text.Printf (printf)
import qualified Interfaces.Logger as In
import Data.Aeson

makeOrderHandler ::(MonadThrow m, IN.Logger m) => MakeOrderUsecase m -> CreateOrderRequest -> m CreateOrderResponse
makeOrderHandler makeOrderUsecase (CreateOrderRequest userId orderItemsRequest) = do
    IN.logDebug logMessage
    resp <- makeOrderUsecase makeOrderDTO
    case resp of
        Left e -> do
            IN.logDebug $ errorMessage e
            throwM $ err404 {errBody = encode (errorMessage e)}
        Right _ -> pure $ CreateOrderResponse "order request accepted"
    where
        fromOrderItemsRequest n = IN.MakeOrderItemDTO (product_id n) (count n)
        orderItems = map fromOrderItemsRequest orderItemsRequest
        makeOrderDTO = IN.MakeOrderDTO userId orderItems
        logMessage = printf "userID:%s orderItemsRequest: %s" (show userId) (show orderItemsRequest) :: String
        errorMessage e = "error: " <> show e
