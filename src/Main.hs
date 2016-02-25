-- Code written by Nathan Coleman. Only reuse/change if you attribute me. 
-- You can contact me at nathantjcc@gmail.com


-- fix: much namespace cluttering in my imports, import qualified or specific functions #namespace
-- fix: divide code up into modules. ENCAPSULATE!

module Main (main) where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo (liftIO)
import Control.Concurrent.MVar

import System.Random
import Control.Monad
import qualified Data.Map as M
import Data.Unique
import Data.List (zipWith4)
import Data.Maybe
import System.IO
import System.Glib.UTFString

--Import my modules:
import World
import Agent
import Weapon
import Targeting
import Render
import Util
import Core

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

-- | Function for setting up the initial world. 
-- fix: ugly initilizing of lists #ugly 
-- todo: difficulty levels
seedWorld :: IO World
seedWorld = do
            let startingCount = 300
                starCount = 1000
            -- TODO: 
            newUniques <- replicateM startingCount $ newUnique
            newThetas <- replicateM startingCount $ randomRIO(-pi,pi)
            newXs <- replicateM startingCount $ randomRIO(-350.0,-50.0)
            newYs <- replicateM startingCount $ randomRIO(-350.0,350.0)
            newSize <- replicateM startingCount $ randomRIO(5.0,10.0)
            newSpeed <- replicateM startingCount $ randomRIO(3,5)
            
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
main :: IO()
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

  {-
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
  -}

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
  windowFullscreen window

  windowSetDefaultSize window (683) 768

  window `on` deleteEvent $ do
         liftIO
             mainQuit
         return False

  widgetShowAll window

  --repeatedTimer (updateWorld world gameArea) ( msDelay 5 )

  --repeatedTimer (do{ o <- get window windowOpacity; postGUIAsync (set window [windowOpacity := max 0.1 (o - 0.001)])}) (msDelay 20)

  --main tick-timer.
  _ <- timeoutAddFull (
        do
            newWorld <- updateWorld world
            --render newWorld gameArea infoLabel bg
            --TODO Calculate time to render last frame, and occasionally skip frames?
            return True
        )
        priorityHighIdle 10


  _ <- timeoutAddFull (
       do
            currentWorld <- readMVar world
            render currentWorld gameArea infoLabel bg
       )
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


parseNewKeyPress :: MVar World -> [Char] -> IO ()
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


parseKeyPress :: MVar World -> MVar (Set.Set KeyVal) -> EventM EKey Bool
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

-- | Calls main function for handling a tick
updateWorld :: MVar World -> IO World
updateWorld world = do
  currentWorld <- takeMVar world
  let newWorld = worldNextStep currentWorld
  putMVar world newWorld
  return newWorld
