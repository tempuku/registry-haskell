module Usecases.Helpers where

import RIO
import qualified RIO.Map as Map
import qualified Interfaces.DAO as IN
import qualified Interfaces.DTO as IN

type EnrichOrderItemsDataWithPrices = IN.ProductPricesMap -> [IN.MakeOrderItemDTO] -> [IN.NewOrderItemDTO]

enrichOrderItemsDataWithPrices :: EnrichOrderItemsDataWithPrices
enrichOrderItemsDataWithPrices productPricesMap =
    map f
    where
        f makeOrderItemDTO = IN.NewOrderItemDTO productId (IN.mOrItCount makeOrderItemDTO) (maybeLookup (Map.lookup productId productPricesMap))
          where
            productId = IN.mOrItProductId makeOrderItemDTO
            maybeLookup Nothing = 0
            maybeLookup (Just val) = val
