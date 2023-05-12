module Usecases where

import RIO
import Services

makeOrderUsecase :: OrdersService m -> MakeOrder m
makeOrderUsecase = _makeOrder
