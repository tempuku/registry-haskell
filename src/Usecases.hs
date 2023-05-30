module Usecases where

import RIO
import Services.Order

makeOrderUsecase :: OrdersService m -> MakeOrder m
makeOrderUsecase = _makeOrder
