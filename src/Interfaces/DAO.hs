module Interfaces.DAO where

import RIO
import qualified Domain.Models as D
import qualified Usecases.Interactors as UC

data Err
  = ErrTechnical
  | ErrValidation
  deriving (Show, Eq)

type ProductPricesMap = Map D.ProductId Float

type GetMap m = (UC.Logger m, MonadIO m) => [D.ProductId] -> m (Either Err ProductPricesMap)

data ProductPricesDAO m = ProductPricesDAO 
    {
        _getMap :: GetMap m
    }
