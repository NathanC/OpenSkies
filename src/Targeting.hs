module Targeting
( TargetSystem (TargetSystem)
, targetSelection
, targetHistory
, defaultTargetSystem
)where

import Data.Unique

data TargetSystem = TargetSystem{
                      targetSelection :: Maybe Unique            
                    , targetHistory   :: [Unique]
                    } 


defaultTargetSystem = TargetSystem Nothing []

--instance Show Unique where
--    show a = show $ hashUnique a 
