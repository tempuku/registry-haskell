module Interfaces.DAO where

import RIO
import qualified Domain.Models as D
import qualified Interfaces.Logger as IN
import qualified Interfaces.DTO as IN

data ErrDAO
  = ErrTechnical String
  | ErrValidation String
  deriving (Show, Eq)

newtype TargetTopic a = TargetTopic {
            _unTTopic :: a
        }

type ProductPricesMap = Map D.ProductId Float

type GetMap m = (IN.Logger m, MonadIO m) => [D.ProductId] -> m (Either ErrDAO ProductPricesMap)

data ProductPricesDAO m = ProductPricesDAO 
    {
        _getMap :: GetMap m
    }

data OrdersDAO m = OrdersDAO {
        _createOrder :: IN.NewOrderDTO -> m (Either ErrDAO D.Order)
    }

data MessagesDAO m a = MessagesDAO 
    {
        _sendNewOrderMsg :: TargetTopic a -> IN.NewOrderMessageDTO -> m (Either ErrDAO ())
    }
