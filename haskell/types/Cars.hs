module Cars
    (
-- import Car data type                         --
--    |  |                                      --
      Car(..)
--       |  |                                   --
-- import all of value constructors for Car     --
    , getSpeed
    ) where

type Brand = String
type Speed = Int

data Car = Car Brand Speed deriving (Show, Eq)

getSpeed :: Car -> Speed
getSpeed (Car _ speed) = speed
