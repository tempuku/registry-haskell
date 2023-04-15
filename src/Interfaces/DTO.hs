module Interfaces.DTO where

import Data.Aeson
import RIO

import qualified Domain.Models as D

--------------Data Access Layer DTOs--------------
data CreateOrderItemDTO = CreateOrderItemDTO {
    crOrItId :: Int,
    crOrItOrderId :: Int
} deriving (Eq, Generic, Show, ToJSON, FromJSON)

--------------Interactors Layer DTOs--------------
data MakeOrderDTO = MakeOrderDTO 
    {
        mOrUserId :: D.UserId,
        mOrOrderItems :: [MakeOrderItemDTO]
    }

data MakeOrderItemDTO = MakeOrderItemDTO {
    mOrItProductId :: D.ProductId,
    mOrItCount :: Int
} deriving (Eq, Generic, Show, ToJSON, FromJSON)

data NewOrderItemDTO = NewOrderItemDTO {
    nOrItProductId :: D.ProductId,
    nOrItCount :: Int,
    nOrItProductPrice :: Float
} deriving (Eq, Generic, Show, ToJSON, FromJSON)

data NewOrderDTO = NewOrderDTO {
    nOrUserId :: D.UserId,
    nOrOrderItems :: [NewOrderItemDTO]
} deriving (Eq, Generic, Show, ToJSON, FromJSON)
