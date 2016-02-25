--This contains my world record type.
--It is in it's own module mainly for the
--purposes of having a clean namespace
--(this should be imported qualified)
--and for cleaner source code.


module World
(  World(World)
, playerRadius
, playerAngle
, playerX
, playerY
, downKey
, upKey   
, leftKey 
, rightKey
, shiftKey
, playerLives 
, tickNumber 
, weaponList  
, weaponFiring 
, agentMap   
, weaponSelected 
, starField   
, playerTargetSystem
, playerScore
, playerHealth
, worldVisible 
, defaultWorld
)where

import Targeting
import Agent
import Weapon
import Data.Unique
import qualified Data.Map as M

data World = World { 
                     playerRadius :: Float 
                   , playerAngle :: Float
                   , playerX :: Float
                   , playerY :: Float
                   , downKey :: Bool
                   , upKey   :: Bool
                   , leftKey :: Bool
                   , rightKey:: Bool
                   , shiftKey :: Bool
                   , playerLives :: Int
                   , tickNumber :: Int
                   , weaponList :: [Weapon] 
                   , weaponFiring   :: Bool
                   , agentMap    :: M.Map Unique Agent  --map of Unique ID's to Agents
                   , weaponSelected :: Missile
                   , starField   :: [(Float,Float,Float)]
                   , playerTargetSystem :: TargetSystem
                   , playerScore :: Int
                   , playerHealth :: Int
                   , worldVisible :: (Int,Int)
                   } 

defaultWorld = World 0 0 0 0 False False False False False 0 0 [] False M.empty StraightMissile [] defaultTargetSystem 0 0 (0,0)
