module Adapter.Storage.InMemory.ProductPrices where

import RIO
import qualified RIO.Map as Map
import qualified Interfaces.DAO as IN
import qualified Usecases.Interactors as UC
import qualified Domain.Models as D

type ProductPricesKVStore = Map D.ProductId Float

getMap :: (MonadIO m) =>  ProductPricesKVStore -> [D.ProductId] -> m (Either IN.Err IN.ProductPricesMap)
getMap productPricesKVStore productIDList = do
    if null productIDList
        then pure (Left IN.ErrValidation)
        else do
            let productPriceList = map toMap productIDList
            if hasNothing productPriceList then pure (Left IN.ErrValidation)
            else do
                let pricesMap = Map.fromList $ zip productIDList (catMaybes productPriceList)
                pure (Right pricesMap)
            where
                toMap productID = Map.lookup productID productPricesKVStore
                hasNothing [] = False
                hasNothing (Nothing:_) = True
                hasNothing (_:xs) = hasNothing xs
                    -- Map.insert (productID (Map.lookup productID productPricesKVStore) pricesMap)

