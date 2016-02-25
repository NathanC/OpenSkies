module Core
(worldNextStep)
where

import GHC.Float
import qualified Data.Map as M
import Data.Unique
import Data.List (minimumBy,foldl',partition)
import Data.Maybe

--Import my modules:
import Radians
import World
import Agent
import Weapon
import Targeting
import Util


moveAgent a = a {agentCoords = updateCoords coords theta speed}
                    where coords = agentCoords a
                          theta  = agentDirection a
                          speed  = agentSpeed a

collisionUpdate :: [Weapon] -> M.Map Unique Agent -> Int -> ([Weapon], M.Map Unique Agent, Int)
collisionUpdate weapons agents score = (remainingWeapons ++ newWeapons, remainingAgents, score+dscore)

     where (remainingWeapons, remainingAgents, newWeapons, dscore) = foldr handleCollision ([], agents, [], 0) weapons

           handleCollision b (remainingWeapons, remainingAgents, newWeapons, dscore) --this is a highly entered inner loop
             | weaponCollision = if isExplosion b
                                   then (b: remainingWeapons, agentsNotHit, newExplosion:newWeapons, dscore+1)
                                   else (remainingWeapons, agentsNotHit, newExplosion:newWeapons, dscore+1)
             | otherwise = (b: remainingWeapons, remainingAgents, newWeapons, dscore)

             where weaponCollision = (not.M.null) agentsHit
                   (agentsHit, agentsNotHit) = M.partition (\a -> agentHit a b) remainingAgents
                   (eX, eY) = agentCoords $ head $ M.elems agentsHit
                   newExplosion = if (isExplosive b)
                                    then (Weapon eX eY 0 0 0 Explosion (120*3) 0)
                                    else (Weapon eX eY 0 0 0 Explosion 120 0)

playerCollisionUpdate :: (Float, Float, Float) -> M.Map Unique Agent -> [Weapon] -> Int -> (M.Map Unique Agent, [Weapon], Int)
playerCollisionUpdate player agents weapons health = (agentsNotInCollision, weapons ++ newWeapons, newHealth)

    where (agentsInCollision, agentsNotInCollision) = M.partition (playerTouchingAgent player) agents
          (weaponsInCollision, weaponsNotInCollision) = partition (playerHitByWeapon player) weapons
          newWeapons = map (\a -> let (eX, eY) = agentCoords a in Weapon eX eY 0 0 0 Explosion 120 0 ) $ M.elems agentsInCollision
          newHealth = max 0 (health - M.size agentsInCollision * 100 - length weaponsInCollision * 3)
          playerHitByWeapon (pX,pY,pR) w
              = circlesTouching (pX,pY,pR) (wX,wY,wR) 1
              where wX = weaponX w
                    wY = weaponY w
                    wR = weaponRadius w
          playerTouchingAgent (pX,pY,pR) a
              = circlesTouching (pX,pY,pR) (aX,aY,aR) 1
              where (aX,aY) = agentCoords a
                    aR      = agentRadius a


-- test to see if an agent, a is in collision with a weapon, b
-- this can be used to have large "weapons" (explosions, lasers)
-- collide with multiple agents
-- can also be used to finely tune what counts as a collision
-- and have special cases depending on the agent/weapon
-- fix: Abstract collisions to be more general
-- add: boundedsquare collision testing? point-in-polygon testing?
agentHit a b = circlesTouching (x1,y1,r1) (x2,y2,r2) 1
               where (x1,y1) = agentCoords a
                     r1      = agentRadius a
                     (x2,y2) = (weaponX b, weaponY b)
                     r2      = weaponRadius b


targetCoords myWorld u = if isJust maybeAgent
                           then Just $ agentCoords $ fromJust maybeAgent
                           else Nothing
                         where maybeAgent = M.lookup u (agentMap myWorld)


--returns the unique ID of the agent closest to the mouse coords for the given world state
-- fix: current algorithm will cause a runtime error if agent list is empty. #high #dangerous #runtime
-- fix: not currently used? Use for heat seeking perhaps? #unused
lockOn myWorld = Just $ fst $ minimumBy closer dataList
                where dataList = map (\(k, a) ->  (k,agentCoords a) )  $ M.toList $ agentMap myWorld
                      closer (_,(x1,y1)) (_,(x2,y2)) = compare (distanceBetweenTwoPoints (x,y) (x1,y1))  (distanceBetweenTwoPoints (x,y) (x2,y2))
                      (x,y) = (playerX myWorld, playerY myWorld)


getSeekingMaybeUnique (Seeking mu) = mu

-- |Function that updates weapon based on the kind of weapon it is
-- fix: weapon name depreciated. Use weapon instead? #high #ugly #style #records #overall
-- fix: Change style for selection based on kind, fix data type? Cases? #high #ugly #overall #records #style
updateweapon :: Weapon -> World ->  Weapon
updateweapon b myWorld
           | isExplosive b =

                let newAge = weaponAge b + 1
                    timer = getExplosiveTimer (typeOfWeapon b)

                in if newAge < timer
                    then b {
                            weaponX = x + deltaX,
                            weaponY = y+deltaY,
                            weaponTheta = theta,
                            weaponSpeed = speed,
                            weaponAge = newAge
                         }

                    else Weapon x y 0 0 0 Explosion (120*3) 0

           | isExplosion b =

                let age = fromIntegral $ weaponAge b
                    ageLimit = fromIntegral $ weaponLifeSpan b
                    newRadius = if age < (ageLimit/2)
                                   then age
                                   else ageLimit - age
                in b {
                    weaponAge = weaponAge b + 1,
                    weaponRadius = newRadius
                }

           | isStraight b  =  b { weaponX = x + deltaX
                                , weaponY = y+deltaY
                                , weaponAge = weaponAge b + 1
                                }

           | isSeeking b =

                let newTheta = if isJust maybeTargetCoords
                                   then shiftTowards
                                            theta
                                            (angleBetweenTwoPoints (x,y) targetCoords )
                                            (max 0.01  (exp (-distanceToTarget/100)))
                                   else theta

                               where distanceToTarget = distanceBetweenTwoPoints (x,y) targetCoords
                                     targetCoords = fromJust maybeTargetCoords

                    maybeTargetCoords = if isNothing maybeTarget
                                            then Nothing
                                            else targetCoords myWorld (fromJust maybeTarget)

                    maybeTarget = getSeekingMaybeUnique $typeOfWeapon b

                in b { weaponX = x + deltaX
                  , weaponY = y+deltaY
                  , weaponTheta = newTheta
                  , weaponSpeed = speed
                  , weaponAge = weaponAge b + 1
                  }

           | otherwise =   b { weaponX = x + deltaX
                              , weaponY = y + deltaY
                              , weaponTheta = theta
                              , weaponSpeed = speed
                              , weaponAge = weaponAge b + 1
                              }

           where theta = weaponTheta b
                 speed = weaponSpeed b
                 deltaX = cos theta * speed
                 deltaY = sin theta * speed
                 x = weaponX b
                 y = weaponY b


removeOldWeapons :: [Weapon] -> World -> [Weapon]
removeOldWeapons bList myWorld
         = if weaponFiring myWorld
             then map (\b -> updateweapon b myWorld) ( filter weaponStillAlive  (newweapon:bList) )
             else map (\b -> updateweapon b myWorld) ( filter weaponStillAlive  bList )

           where newweapon
                     |currentWeapon == SeekingMissile  = Weapon newweaponX newweaponY 4.0 newweaponTheta 8 (Seeking (targetSelection (playerTargetSystem myWorld))) 1500 1
                     |currentWeapon == StraightMissile = Weapon newweaponX newweaponY 4.0 newweaponTheta 20 Straight 500 1
                     |currentWeapon == HeatSeekingMissile = Weapon newweaponX newweaponY 4.0 newweaponTheta 10 HeatSeeking 500 1
                     |currentWeapon == ExplosiveMissile = Weapon newweaponX newweaponY 4.0 newweaponTheta 6 (Explosive 150) 500 1
                     |otherwise = Weapon newweaponX newweaponY 1.0 newweaponTheta 2 Straight 500 1


                 currentWeapon = weaponSelected myWorld
                 newweaponX = myCannonTipX
                 newweaponY = myCannonTipY
                 newweaponTheta = myCannonAngle
                 myCannonAngle = playerAngle myWorld
                 myCannonTipX = pX + cos myCannonAngle * 15
                 myCannonTipY = pY + sin myCannonAngle * 15

                 pX = playerX myWorld
                 pY = playerY myWorld

                 weaponStillAlive :: Weapon -> Bool
                 weaponStillAlive b = weaponAge b < weaponLifeSpan b

worldNextStep :: World -> World
worldNextStep myWorld =

    let speed = 8
        xShift = if up
                    then cos newTheta * speed
                    else 0
        yShift = if up
                    then sin newTheta * speed
                    else 0

        newTheta = combineAngles (playerAngle myWorld) $ thetaChange left right

        right = rightKey myWorld
        left  = leftKey myWorld
        down  = downKey myWorld
        up    = upKey myWorld
        turnSpeed = 0.1


        -- |Takes a left and right
        thetaChange False False = 0
        thetaChange True  False = turnSpeed
        thetaChange False True = -turnSpeed
        thetaChange True True =  0

        agents' = M.map moveAgent (agentMap myWorld)

        weapons' = removeOldWeapons (weaponList myWorld) myWorld

        (weapons'', agents'', score') = collisionUpdate weapons' agents' (playerScore myWorld)

        (agents''', weapons''', health') = playerCollisionUpdate (playerX myWorld, playerY myWorld, playerRadius myWorld) agents'' weapons'' (playerHealth myWorld)

    in myWorld { playerX = playerX myWorld + xShift
                   , playerY = playerY myWorld + yShift
                   , playerAngle = newTheta
                   , tickNumber = tickNumber myWorld + 1
                   , weaponList = weapons'''
                   , weaponFiring = False
                   , agentMap = agents'''
                   , playerScore = score'
                   , playerHealth = health'
                   }

