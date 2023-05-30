module Interfaces.Usecases where

import RIO
import Services.Order

type MakeOrderUsecase m = MakeOrder m


data Usecases m = Usecases {
    _makeOrderUsecase :: MakeOrderUsecase m
}
