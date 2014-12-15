-- Code written by Nathan Coleman. Only reuse/change if you attribute me. 
-- You can contact me at nathantjcc@gmail.com


-- fix: much namespace cluttering in my imports, import qualified or specific functions #namespace
-- fix: divide code up into modules. ENCAPSULATE!

module Main (main) where

import GHC.Float

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Gdk.EventM
import Control.Concurrent.MVar

import System.Random
import Control.Monad
import qualified Data.Map as M
import Data.Unique
import Data.List (zipWith4, minimumBy,foldl',partition)
import Data.Maybe
import System.IO
import System.Glib.UTFString

--import Control.Concurrent.Timer
--import Control.Concurrent.Suspend.Lifted
import Control.Concurrent



--Import my modules:
import Radians
import World
import Agent
import Weapon
import Targeting

--cabal path module
import Paths_OpenSkies


import qualified Data.Set as Set




-- Instance of show for the imported Unique type
-- Main purpose of this to allow World record to derive show
--instance Show Unique where
--    show a = show $ hashUnique a 










-- | This function saves the score to the high-score file, called highscores.txt
--   I have it take myWorld instead of specific fields, so I can customize this 
--   function easier without changing how the rest of my code uses it
--   This function takes data from the world, but doesn't change the world

saveScore :: World -> IO ()
saveScore myWorld = do
    hsFile <- openFile "highscores.txt" AppendMode
    hPutStrLn hsFile (show s ++ "  " ++ name)
    hClose hsFile

    where s = playerScore myWorld
          name = "Nathan"



-- | Checks to see if two points are within a given distance
pointsWithinDistance (x1,y1) (x2,y2) distance = xDif^2 + yDif^2 <= distance ^ 2
                                                where xDif = x2-x1
                                                      yDif = y2-y1            
-- | returns the distance between two points
distanceBetweenTwoPoints (x1,y1) (x2,y2) = sqrt( (x2-x1)^2 + (y2-y1)^2)


-- | Returns True if the two given circles are touching, with a given error range.
circlesTouching (x1,y1,r1) (x2,y2,r2) err = pointsWithinDistance (x1,y1) (x2,y2) (r1+r2-err)

updateCoords (x,y) theta speed 
    = (x+deltaX,y+deltaY)
    where deltaX = cos theta * speed 
          deltaY = sin theta * speed                    

updateAgent a = a{agentCoords = updateCoords coords theta speed} 
                    where coords = agentCoords a
                          theta  = agentDirection a
                          speed  = agentSpeed a





    
collisionUpdate weapons agents score 
     = (dweapons ++ weapons',agents',score+dscore)
           
     where (weapons',agents',dweapons,dscore)  = foldl handleCollision ([],agents,[],0)  weapons
           
           handleCollision (weapons',agents',dweapons,dscore) b 
             | weaponCollision = if isExplosion b
                                   then (weapons'++[b],agentsNotHit,newExplosion:dweapons,dscore+1)
                                   else (weapons',agentsNotHit,newExplosion:dweapons,dscore+1)
             | otherwise = ( weapons'++[b],agents',dweapons,dscore) -- conserves the order of the weapon list, but more expensive then : operator
             
             where weaponCollision = (not.M.null) agentsHit
                   (agentsHit,agentsNotHit) = M.partition (\a -> agentHit a b) agents'
                   (eX,eY) = agentCoords $ head $ M.elems agentsHit
                   newExplosion = if (isExplosive b) 
                                    then (Weapon eX eY 0 0 0 Explosion (120*3) 0)
                                    else (Weapon eX eY 0 0 0 Explosion 120 0)



playerCollisionUpdate player agents weapons health
    = ( agentsNotInCollision, newWeapons ++ weapons , newHealth )

    where (agentsInCollision,agentsNotInCollision) = M.partition (playerHitByAgent player) agents
          (weaponsInCollision,weaponsNotInCollision) = partition (playerHitByWeapon player) weapons
          newWeapons =  map (\a -> let (eX,eY) = agentCoords a in Weapon eX eY 0 0 0 Explosion 120 0 ) $ M.elems agentsInCollision 
          newHealth  =  max 0 (health - M.size agentsInCollision * 100 - length weaponsInCollision * 3)   
          playerHitByWeapon (pX,pY,pR) w
              = circlesTouching (pX,pY,pR) (wX,wY,wR) 1
              where wX = weaponX w
                    wY = weaponY w
                    wR = weaponRadius w
          playerHitByAgent (pX,pY,pR) a
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
updateweapon ::  Weapon -> World ->  IO Weapon
updateweapon b myWorld 

           | isExplosive b = do

                     let deltaX = cos theta * speed
                         deltaY = sin theta * speed
                             
                         timer = getExplosiveTimer (typeOfWeapon b)
                         newAge = weaponAge b + 1 
                     
                     if newAge < timer
                        then return b { weaponX = x + deltaX
                                      , weaponY = y+deltaY
                                      , weaponTheta = theta
                                      , weaponSpeed = speed
                                      , weaponAge = newAge                             
                                      } 
                        else return $ (Weapon x y 0 0 0 Explosion (120*3) 0)

           | isExplosion b = do
                      
                     let age = fromIntegral $ weaponAge b
                         ageLimit = fromIntegral $ weaponLifeSpan b
                         newRadius = if age < (ageLimit/2)    
                                       then age
                                       else ageLimit - age            
                    

                     return b { weaponAge = weaponAge b + 1
                              , weaponRadius = newRadius
                              } 


           | isStraight b  =  do
                        
                     
                     let deltaX = cos theta * speed
                         deltaY = sin theta * speed
                         
                     return b { weaponX = x + deltaX
                              , weaponY = y+deltaY
                              , weaponAge = weaponAge b + 1
                              } 

            | isSeeking b = do

                  
                     let deltaX = cos newTheta * speed
                         deltaY = sin newTheta * speed
                                                
                         
                         newTheta = if isJust maybeTargetCoords 
                                     then shiftTowards theta (angleBetweenTwoPoints (x,y) targetCoords ) (max 0.01  (exp (-distanceToTarget /100)))
                                            
                                     else theta    
                                            where distanceToTarget = distanceBetweenTwoPoints (x,y) targetCoords
                                                  targetCoords = fromJust maybeTargetCoords
                         
                         maybeTargetCoords = if isNothing maybeTarget
                                                then Nothing
                                                else targetCoords myWorld (fromJust maybeTarget)       
                         
                         maybeTarget = getSeekingMaybeUnique $typeOfWeapon b
               
                        
                     return b { weaponX = x + deltaX
                              , weaponY = y+deltaY
                              , weaponTheta = newTheta
                              , weaponSpeed = speed
                              , weaponAge = weaponAge b + 1
                              } 

           | otherwise = do
                   
                     let deltaX = cos theta * speed
                         deltaY = sin theta * speed
                         
                        
                     return b { weaponX = x + deltaX
                              , weaponY = y+deltaY
                              , weaponTheta = theta
                              , weaponSpeed = speed
                              , weaponAge = weaponAge b + 1                              
                              } 

           where theta = weaponTheta b
                 speed = weaponSpeed b 
                 x = weaponX b
                 y = weaponY b 



updateweaponList :: [Weapon] -> World -> IO [Weapon]
updateweaponList bList myWorld  
        = do
           updatedList <- if weaponFiring myWorld
                           then mapM (\b -> updateweapon b myWorld) ( filter weaponStillAlive  (newweapon:bList) )
                           else mapM (\b -> updateweapon b myWorld) ( filter weaponStillAlive  bList )
           return updatedList
           where newweapon 
                     |currentWeapon == SeekingMissile  = Weapon newweaponX newweaponY 4.0 newweaponTheta 4 (Seeking (targetSelection (playerTargetSystem myWorld))) 1500 1                
                     |currentWeapon == StraightMissile = Weapon newweaponX newweaponY 4.0 newweaponTheta 20 Straight 500 1
                     |currentWeapon == HeatSeekingMissile = Weapon newweaponX newweaponY 4.0 newweaponTheta 5 HeatSeeking 500 1
                     |currentWeapon == ExplosiveMissile = Weapon newweaponX newweaponY 4.0 newweaponTheta 3 (Explosive 150) 500 1
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



worldNextStep :: World -> IO World
--fix: do NOT hang, perhaps just return myWorld? Should be handled before updating the world? 
--worldNextStep myWorld
--    |playerHealth myWorld <= 0  = do 
--                                    saveScore myWorld
--                                    b <- hWaitForInput stdin (-1)
--                                    return myWorld





worldNextStep myWorld = do

    updatedweaponList <- updateweaponList (weaponList myWorld) myWorld                            
    let speed = 2                
        xShift = if up 
                    then cos newTheta * speed     
                    else 0
        yShift = if up
                    then sin newTheta * speed
                    else 0      
 
        newTheta = combineAngles (playerAngle myWorld) $ thetaChange left right
               
        
        updatedAgentMap = M.map updateAgent (agentMap myWorld)

 
        
        (newweaponList,newAgentMap,newScore) = collisionUpdate updatedweaponList updatedAgentMap (playerScore myWorld)

        (finalAgentMap,finalweaponList, finalHealth) = playerCollisionUpdate (playerX myWorld, playerY myWorld, playerRadius myWorld)  newAgentMap newweaponList (playerHealth myWorld)

         
    return myWorld { playerX = playerX myWorld + xShift
                   , playerY = playerY myWorld + yShift 
                   , playerAngle = newTheta
                   , tickNumber = tickNumber myWorld + 1
                   , weaponList = finalweaponList           
                   , weaponFiring = False
                   , agentMap = newAgentMap
                   , playerScore = newScore
                   , playerHealth = finalHealth
                   }

    where right = rightKey myWorld
          left  = leftKey myWorld
          down  = downKey myWorld
          up    = upKey myWorld



          -- |Takes a left and right
          thetaChange False False = 0
          thetaChange True  False = 0.05
          thetaChange False True = -0.05
          thetaChange True True =  0






-- |Selects a target on screen, that has not been recently selected.
-- fix: range for "inView" incorrect, should use current visible game-gameArea size
-- add: 
selectTarget :: World -> TargetSystem
selectTarget myWorld
     |(not.null) targetsAvailable =  TargetSystem (Just newTarget) (newTarget:alreadyUsed)
     | (not.null) alreadyUsed     =  TargetSystem (Just (last alreadyUsed))  (last alreadyUsed : init alreadyUsed)
     | otherwise =  TargetSystem Nothing []
             
     where newTarget = head targetsAvailable
           targetsAvailable = filter (\u ->  not (u `elem` alreadyUsed)) allInView       
           allInView = M.keys $ M.filter inView $ agentMap myWorld
           alreadyUsed =  filter (stillInView) $ targetHistory $ playerTargetSystem myWorld
           inView a = (x > (-width/2+pX) && x < (width/2+pX)) && (y > (-height/2+pY) && y < (height/2+pY))
                      where (x,y) = agentCoords a
                            (pX,pY) = (playerX myWorld, playerY myWorld)
           (width,height) = (fromIntegral widthRaw, fromIntegral heightRaw)
           (widthRaw,heightRaw) = worldVisible myWorld
           stillInView k = if isNothing maybeAgent
                             then False
                             else inView (fromJust maybeAgent)
                           where maybeAgent = M.lookup k (agentMap myWorld)


--successor function that loops through bounded types
succ' :: (Bounded a, Eq a, Enum a) => a -> a
succ' n
 | n == maxBound = minBound
 | otherwise = succ n

--predecesor function that loops through bounded types
pred' :: (Bounded a, Eq a, Enum a) => a -> a
pred' n
 | n == minBound = maxBound
 | otherwise = pred n


-- | Function for setting up the initial world. 
-- fix: ugly initilizing of lists #ugly 
-- todo: difficulty levels
seedWorld = do
            
            let startingCount = 300
                starCount = 1000
            
            newUniques <- replicateM startingCount $ newUnique
            newThetas <- replicateM startingCount $ randomRIO(-pi,pi)
            newXs <- replicateM startingCount $ randomRIO(-350.0,-50.0)
            newYs <- replicateM startingCount $ randomRIO(-350.0,350.0)
            newSize <- replicateM startingCount $ randomRIO(5.0,10.0)
            newSpeed <- replicateM startingCount $ randomRIO(0.5,2)
            
            starXs <- replicateM starCount $ randomRIO(-4000,4000)
            starYs <- replicateM starCount $ randomRIO(-4000,4000)
            starRs <- replicateM starCount $ randomRIO(2,9)


            let newCoords = zip newXs newYs
                constructAgent coords d s r = Agent coords [] d s r
                newAgents = zipWith4 constructAgent newCoords newThetas newSpeed newSize
                newStarList  = zip3 starXs starYs starRs
               

            return $ World 10  0 0 0 False False False False False 3 0  [] False (M.fromList (zip newUniques newAgents ))
                           StraightMissile newStarList (TargetSystem Nothing []) 0 1000 (0,0)






-- |Main function, also the function that contains the gtk main loop
-- fix: separate main and gtk-main-loop
--      use clearer variable names  #style      
--  

main = do
  initGUI
  

  builder <- builderNew

  gladePath <- getDataFileName "resources/OpenSkies.glade"
  builderAddFromFile builder gladePath
  
  window <- builderGetObject builder castToWindow "mainWindow"
  
  gameArea <- builderGetObject builder castToDrawingArea "gameArea"
  infoLabel <- builderGetObject builder castToLabel "infoLabel"

     
  initalWorld <- seedWorld 
    
  world <- newMVar initalWorld 
  
  currentKeysDepressed <- newMVar Set.empty


  --bg <- pixbufNewFromFile "Space-Stars-Background.jpg"
  --bg <- pixbufNewFromFile "Shiptest.png"
  filePath <- getDataFileName "resources/Space-Background.jpg"
  bg <- pixbufNewFromFile filePath
  --bg <- return "hello"


  --TODO: expose event obsolete
  gameArea `on` exposeEvent $ do
    r <- eventArea       

    liftIO $ do
        
        w <- readMVar world
        let (width,height) = worldVisible w    
    
        win' <- widgetGetWindow gameArea   
        
        if isNothing win'
           then do{putStrLn "Widget not realized, error.";mainQuit}
           else return ()

        let win = fromJust win'

        drawWindowBeginPaintRect win r

        renderWithDrawWindow win $ renderWorld w (fromIntegral width)  (fromIntegral height) bg
  
        drawWindowEndPaint win 

    return True



  gameArea `on` configureEvent $ do
    (width,height) <- eventSize    
    
    liftIO $ do
        modifyMVar_ world (\w -> return (w{worldVisible = (width,height)}) )

            
    return True
    

  --here are the defined event handlers  
  window `on` keyPressEvent $ parseKeyPress world currentKeysDepressed
  window `on` keyReleaseEvent $ parseKeyRelease world currentKeysDepressed
                               

  gameOverMessage <- messageDialogNew (Nothing) [] MessageInfo ButtonsOk "Game over."
  
  windowSetGravity window GravityNorthWest
  

  windowMove window 1366 0
  --windowFullscreen window

  windowSetDefaultSize window (683) 768

  window `on` deleteEvent $ do
         liftIO
             mainQuit
         return False

  widgetShowAll window

  --repeatedTimer (updateWorld world gameArea) ( msDelay 5 )

  --repeatedTimer (do{ o <- get window windowOpacity;postGUIAsync (set window [windowOpacity := max 0.1 (o - 0.001)])}) (msDelay 20)
                    

  --main tick-timer.
  _ <- timeoutAddFull (updateWorld world window gameArea infoLabel bg)
                  priorityDefaultIdle 10
  
  
 
  --dialogRun gameOverMessage
  --widgetDestroy gameOverMessage

  


  mainGUI



-- fix: For all following keyboard press/release callbacks, needless overhead is used in
--      string comparison. I should instead hardcode the keyVals.
--      Or, for that matter, switch to using the set of depressed keys
--      Perhaps have world/set of keys/various other "global" io MVars
--      be accessible in IO functions without passing the handle to the MVars?


        
-- | Callback function for when a key has been
--   released. Removes the key from the set
--   of depressed keys, and changes world-flags
-- fix: redundant use of flags/keycode set?
-- add: 
       



parseKeyRelease :: MVar World -> MVar (Set.Set KeyVal) -> EventM EKey Bool
parseKeyRelease world currentKeysDepressed = do
            
            kv <- eventKeyVal
            k <- eventKeyName    
            let kString = glibToString k            

            liftIO $ do
               

                modifyMVar_ currentKeysDepressed (\c -> return (Set.delete kv c))
                    
                w <- takeMVar world 
             

                case kString of 
                    "w" -> putMVar world $! w {upKey = False}
                    "a" -> putMVar world $! w {leftKey = False}
                    "s" -> putMVar world $! w {downKey = False}
                    "d" -> putMVar world $! w {rightKey = False} 
                    "Up" -> putMVar world $! w {upKey = False}
                    "Left" -> putMVar world $! w {leftKey = False}
                    "Down" -> putMVar world $! w {downKey = False}
                    "Right" -> putMVar world $! w {rightKey = False} 
                    _  -> putMVar world $! w   

                putStrLn $ "key released: " ++  kString
            
            return True




parseNewKeyPress world kString = do
            
            putStrLn $ "key pressed: " ++ kString
      
                 
            w <- takeMVar world 

      
            case kString of 
                "w" -> putMVar world $! w {upKey = True}
                "a" -> putMVar world $! w {leftKey = True}
                "s" -> putMVar world $! w {downKey = True}
                "d" -> putMVar world $! w {rightKey = True} 
                "Up" -> putMVar world $! w {upKey = True}
                "Left" -> putMVar world $! w {leftKey = True}
                "Down" -> putMVar world $! w {downKey = True}
                "Right" -> putMVar world $! w {rightKey = True} 
                "space" -> putMVar world $! w {weaponFiring = True}
                "x"     -> putMVar world $! w {weaponSelected = succ' (weaponSelected w)}
                "Tab"   -> putMVar world $! w {playerTargetSystem = selectTarget w}
                "Escape" -> do{putStrLn "Goodbye!"; putMVar world w; mainQuit}  --not sure if I should or should not putMVar back. Seems good form to?
                _  -> putMVar world $! w    


                


parseKeyPress world currentKeysDepressed = do
           
            
            --prevent blocking of mvar by having another mvar set that keeps track of which keys are depressed, and doesn't call an event on 
            --those keys. Something like if(not in set) then add to set and do liftIO functions else return True (and remove from set if depressed)

            --bit of overhead, but doesn't block MVar. Perhaps MVar wouldn't be blocked anyways, because updateWorld event not called
            --until this finishes? This may or may not help. Seems cleaner though.


            

            kv <- eventKeyVal 
            k  <- eventKeyName
            let kString = glibToString k         
    
            liftIO $ do 
                c <- takeMVar currentKeysDepressed
                

                if Set.member kv c 
                   then do{putMVar currentKeysDepressed c}               
                   else do{putMVar currentKeysDepressed (Set.insert kv c);parseNewKeyPress world kString}
                                  
                    
            return True
                
                   

--my render standards:
-- all(?) my render functions should save and restore context to make them not impact the global context
-- the level of granulity for which to not combine contexts is still to be decided
-- Keep simple Render() return types, avoid IO monad. If nessesary, random numbs generated
-- through a passed stdGen

-- fix: coordinate system "wrong". #ugly #messy #high
-- fix: too much rework. Create items, then rotate/translate/render them. #efficency #speed #bottleneck
-- fix: get/set the matrix to save time #bottleneck #speed 



renderFlame w = do
    let x = float2Double $ playerX w
        y = float2Double $ playerY w
        r = float2Double $ playerRadius w
        
    save

    renderFlameState r $ tickNumber w `mod` 3
        
    
    restore    

    where renderFlameState r s 
                | s == 1 = do 
                    setSourceRGBA 1 0 0 0.8

                    moveTo (-r) (0)
                    lineTo (-r-10) (-5)
                    lineTo (-r-10) (5)    

                    fill
                | s == 2 = do 
                    setSourceRGBA 1 0 0 0.8

                    moveTo (-r) (0)
                    lineTo (-r-7) (-5)
                    lineTo (-r-7) (5)    

                    fill        
            

                | s == 3 = do
                    setSourceRGBA 1 0 0 0.8

                    moveTo (-r) (0)
                    lineTo (-r-11) (-5)
                    lineTo (-r-11) (5)    

                    fill

                | otherwise = do    
                    setSourceRGBA 0.8 0.5 0.5 0.8

                    moveTo (-r) (0)
                    lineTo (-r-8) (-5)
                    lineTo (-r-8) (5)    

                    fill


renderWorld myWorld width height bg = do
    

    let x = float2Double $ playerX myWorld
        y = float2Double $ playerY myWorld
        r = float2Double $ playerRadius myWorld
        pTheta = playerAngle myWorld
        myCannonTipX = float2Double $ cos pTheta * 10
        myCannonTipY = float2Double $ sin pTheta * 10 
        

    setSourceRGB 0 0 0
    --setSourcePixbuf bg (-x - 200) (y - 200)
    paint

    

    translate (width / 2) (height / 2)
    --at this point, the screen is centered at 0
    --gloss x = x, gloss y = (-y)
    --will fix this.


    
    setLineWidth 1



    --render the player 
    setSourceRGB 1 0 0
   

    
    save

    rotate (- (float2Double pTheta))

    setSourceRGB 0.30 0.30 0.34

    moveTo (r*1.5)0
    lineTo (-9) (-r-3) 
    lineTo (-4) 0
    lineTo (-9)  (r+3)
    lineTo (r*1.5) 0
    fillPreserve
    
    save
    
    setSourceRGB 1 1 1 
    stroke
    
    restore
    

    arc 0 0 r 0 (2*pi)
    
    fillPreserve
    
    save
    setSourceRGB 1 1 1
    stroke
    restore

    
    arc 0 0 (0.7 * r) 0  (2*pi)
    clip

    setSourceRGB 1 1 1
    
    arc (0+1.3*r) 0 r  (0) (2*pi)
    fill
    
    resetClip

    if upKey myWorld
        then renderFlame myWorld
        else return ()


    {-        
    moveTo (-5) (-5)
    lineTo (0+*5) 0
    lineTo (-5) (5)
    lineTo (-3) 0
    -} 


    fill
    restore
    

    save
    translate (-x) y
    
    renderTargetCrosshair myWorld
    
    setSourceRGB 0 1 0

    renderAgents $ M.elems (agentMap myWorld)
    
    setSourceRGB 0 0 1
    
    renderWeapons $ weaponList myWorld       
    
    restore

    



renderTargetCrosshair myWorld 
    | isNothing targetedAgent = return ()
    | isNothing maybeAgent    = return ()
    | otherwise =  do 
                     arc (float2Double x) (float2Double (-y)) ((float2Double r)+10) 0 (pi*2)
                     stroke

      where (x,y) = agentCoords (fromJust maybeAgent)
            r     = agentRadius (fromJust maybeAgent)
            targetedAgent =  targetSelection $ playerTargetSystem myWorld
            maybeAgent = M.lookup (fromJust targetedAgent) (agentMap myWorld)      
            
            




renderWeapons [] = return ()
renderWeapons (w:ws) 
    | isSeeking w = 
        do
            save
    
            setSourceRGB 0 0 1 
    
            translate x (-y)
            rotate (-t)
            moveTo (-12) (-2)
            lineTo  (r)  0
            lineTo (-12)  2 
            fill
    
            restore        
            renderWeapons ws
        
    | isExplosion w = 
        do 
            save
            
            setSourceRGBA 0.9 0.1 0.1 0.5
    
            arc x (-y) r 0 (pi*2)
            fillPreserve
        
            setSourceRGB 0 0 0
            stroke
    
            restore        
            renderWeapons ws
            
        
    | isExplosive w = 
        do
            save
            setSourceRGB 0.9 0.4 0.1
    
            translate x (-y)
            rotate (-t)
            moveTo (-12) (-2)
            lineTo  (r)  0
            lineTo (-12)  2 
            fill    

            restore        
            renderWeapons ws
        
        
    | isHeatSeeking w = 
        do
            save
            setSourceRGB 255 255 255
    
            translate x (-y)
            rotate (-t)
            moveTo (-12) (-2)
            lineTo  (r)  0
            lineTo (-12)  2 
            fill
            restore        
            renderWeapons ws        
        
        
    | isStraight w = 
        do
            save

            setSourceRGB 0 1 1
    
            translate x (-y)
            rotate (-t)
            moveTo (-12) (-2)
            lineTo  (r)  0
            lineTo (-12)  2 
            fill

            restore        
            renderWeapons ws
        
        
    | otherwise  = 
        do
            save
            setSourceRGB 0 1 1
    
            translate x (-y)
            rotate (-t)
            moveTo (-12) (-2)
            lineTo  (r)  0
            lineTo (-12)  2 
            fill    

            restore        
            renderWeapons ws        
    
    where x = float2Double $ weaponX w
          y = float2Double $ weaponY w
          r = float2Double $ weaponRadius w       
          t = float2Double $ weaponTheta w                             


renderAgents [] = return ()
renderAgents agents = 
  do
    save
    setSourceRGB 0.30 0.30 0.34
    translate (x) (-y)
    rotate (-t) 

            
    moveTo (-r) (-r/2)
    lineTo (r)  (-r/2)
    lineTo (r)  (r/2)
    lineTo (-r) (r/2)
    fill
    
    moveTo r (r/2)
    lineTo (r+(r/2)) (r)
    lineTo (-(r+(r/2))) (r)
    lineTo (-r) (r/2)
    stroke 

    
    moveTo (-r) (-r/2)
    lineTo (-(r+(r/2))) (-r)
    lineTo (r+(r/2)) (-r)
    lineTo (r) (-r/2)
    stroke 
    
    
    arc (r) 0 (r/2) (-pi/2) (pi/2) 

    strokePreserve

    setSourceRGB 1 1 1

    fill

    --setSourceRGB 1 1 1
    --arc 0 0 (r) 0 (2*pi)    
    --stroke
    restore   
    
    renderAgents (tail agents)  
  where agent = head agents
        (x,y) = (\(x,y) -> (float2Double x, float2Double y)  ) $ agentCoords agent
        r     = float2Double $ agentRadius agent
        t     = float2Double $ agentDirection agent



-- | Utlity function
-- fix: elaborate, possibly put in module/qualify, see if there is a more efficent painting proccess? #drawing #bottleneck #speed
drawWindowBeginPaintFull win = do 
    width <- drawWindowGetWidth win
    height <- drawWindowGetHeight win

    drawWindowBeginPaintRect win (Rectangle 0 0 width height)



-- | Main function for handling a tick.
--   Updates the world, then renders 
--   the information
updateWorld world window gameArea infoLabel bg = do
  currentWorld <- takeMVar world 
  newWorld <- worldNextStep currentWorld 
  putMVar world newWorld 
  
  gameWin' <- widgetGetWindow gameArea
  
  if isNothing gameWin'
    then do{putStrLn "Widget not realized, error.";mainQuit}
    else return ()

  let gameWin = fromJust gameWin'



    
  width' <- drawWindowGetWidth gameWin
  height' <- drawWindowGetHeight gameWin

  let width  = realToFrac width'
      height = realToFrac height'
      x = float2Double $ playerX newWorld
      y = float2Double $ playerY newWorld


  
  
  labelSetText infoLabel  $ "Health: " ++ show (playerHealth newWorld) ++
                            "\nScore: " ++ show (playerScore newWorld) ++
                            "\nWeapon selected: " ++ show  (weaponSelected newWorld)                     


  drawWindowBeginPaintRect gameWin (Rectangle 0 0 width' height')

  renderWithDrawWindow gameWin $ renderWorld newWorld width height bg
   
  drawWindowEndPaint gameWin



  return True
     
     

