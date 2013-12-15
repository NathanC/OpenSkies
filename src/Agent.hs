

module Agent
( Agent(Agent)
, agentCoords
, agentWeaponList
, agentDirection
, agentSpeed
, agentRadius
, agentX
, agentY 
)where 

import Weapon

agentX a = fst $ agentCoords a
agentY a = snd $ agentCoords a

data Agent  = Agent  { agentCoords     :: (Float, Float)
                      ,agentWeaponList :: [Weapon]
                      ,agentDirection  :: Float
                      ,agentSpeed      :: Float
                      ,agentRadius     :: Float
                     } deriving (Eq)
