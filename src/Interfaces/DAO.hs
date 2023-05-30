module Interfaces.DAO where

import RIO
import qualified Domain.Models as D
import qualified Interfaces.Logger as IN
import qualified Interfaces.DTO as IN

data ErrDAO
  = ErrTechnical String
  | ErrValidation String
  deriving (Show, Eq)

type ProductPricesMap = Map D.ProductId Float

type GetMap m = (IN.Logger m, MonadIO m) => [D.ProductId] -> m (Either ErrDAO ProductPricesMap)

data ProductPricesDAO m = ProductPricesDAO 
    {
        _getMap :: GetMap m
    }

data OrdersDAO m = OrdersDAO {
        _createOrder :: IN.NewOrderDTO -> m (Either ErrDAO D.Order)
    }

data MessagesDAO m = MessageDAO 
    {
        _sendNewOrderMsg :: IN.NewOrderMessageDTO -> m (Either ErrDAO ())
    }
