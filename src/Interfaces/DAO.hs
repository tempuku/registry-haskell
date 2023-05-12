module Interfaces.DAO where

import RIO
import qualified Domain.Models as D
import qualified Interfaces.Logger as IN

data Err
  = ErrTechnical
  | ErrValidation
  deriving (Show, Eq)

type ProductPricesMap = Map D.ProductId Float

type GetMap m = (IN.Logger m, MonadIO m) => [D.ProductId] -> m (Either Err ProductPricesMap)

data ProductPricesDAO m = ProductPricesDAO 
    {
        _getMap :: GetMap m
    }
