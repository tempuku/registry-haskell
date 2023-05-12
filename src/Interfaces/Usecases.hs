module Interfaces.Logger where

import RIO
import Services


data Usecases m = Usecases {
    _makeOrderUsecase :: MakeOrder m
}
