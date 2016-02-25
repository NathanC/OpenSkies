module Render (
renderWorld, -- deprecate
render
) where

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo hiding (width, height, x, y)
import GHC.Float
import qualified Data.Map as M
import Data.Maybe

import World
import Weapon
import Agent
import Targeting

--TODO:  Change this to some more sprite-based rendering system. Clarify rendering specs, and make sure totally isolated

--my render standards:
-- all(?) my render functions should save and restore context to make them not impact the global context
-- the level of granulity for which to not combine contexts is still to be decided
-- Keep simple Render() return types, avoid IO monad. If necessary, random numbs generated
-- through a passed stdGen

-- fix: coordinate system "wrong". #ugly #messy #high
-- fix: too much rework. Create items, then rotate/translate/render them. #efficency #speed #bottleneck
-- fix: get/set the matrix to save time #bottleneck #speed

render :: World -> DrawingArea -> Label -> Pixbuf -> IO Bool
render world gameArea infoLabel bg = do
  gameWin' <- widgetGetWindow gameArea

  if isNothing gameWin'
    then do{putStrLn "Widget not realized, error."; mainQuit}
    else return ()

  let gameWin = fromJust gameWin'

  width' <- drawWindowGetWidth gameWin
  height' <- drawWindowGetHeight gameWin

  let width  = realToFrac width'
      height = realToFrac height'

  labelSetText infoLabel  $ "Health: " ++ show (playerHealth world) ++
                            "\nScore: " ++ show (playerScore world) ++
                            "\nWeapon selected: " ++ show (weaponSelected world) ++
                            "\nTick: " ++ show (tickNumber world)

  drawWindowBeginPaintRect gameWin (Rectangle 0 0 width' height')
  renderWithDrawWindow gameWin $ renderWorld world width height bg
  drawWindowEndPaint gameWin

  return True


renderWorld :: World -> Double -> Double -> Pixbuf -> Render()
renderWorld myWorld width height bg = do

  let x = float2Double $ playerX myWorld
      y = float2Double $ playerY myWorld

  setSourceRGB 0 0 0 --space! Default stroke color is black
  setLineWidth 1

  --setSourcePixbuf bg (-x - 200) (y - 200)
  paint

  --translate the coordinate system so that (0,0) is in the middle of the screen, not the top left
  translate (width / 2) (height / 2)

  renderShip myWorld

  --translate future renders by player coordinates. Essentially creates a viewport centered on my player.
  translate (-x) y

  renderTargetCrosshair myWorld
  renderAgents $ M.elems (agentMap myWorld)
  renderWeapons $ weaponList myWorld


renderShip :: World -> Render()
renderShip w = do
    let r = float2Double $ playerRadius w
        pTheta = playerAngle w


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

    if upKey w
        then renderFlame w
        else return ()

    fill


    restore


renderFlame :: World -> Render()
renderFlame w = do
    let r = float2Double $ playerRadius w

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


renderTargetCrosshair :: World -> Render()
renderTargetCrosshair myWorld
    | isNothing targetedAgent = return ()
    | isNothing maybeAgent    = return ()
    | otherwise =  do
                     save
                     setSourceRGB 1 0 0
                     arc (float2Double x) (float2Double (-y)) ((float2Double r)+10) 0 (pi*2)
                     stroke
                     restore

      where (x,y) = agentCoords (fromJust maybeAgent)
            r     = agentRadius (fromJust maybeAgent)
            targetedAgent =  targetSelection $ playerTargetSystem myWorld
            maybeAgent = M.lookup (fromJust targetedAgent) (agentMap myWorld)





renderWeapons :: [Weapon] -> Render()
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


renderAgents :: [Agent] -> Render()
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


-- | Utility function
-- fix: elaborate, possibly put in module/qualify, see if there is a more efficent painting proccess? #drawing #bottleneck #speed
drawWindowBeginPaintFull :: DrawWindow -> IO()
drawWindowBeginPaintFull win = do
    width <- drawWindowGetWidth win
    height <- drawWindowGetHeight win

    drawWindowBeginPaintRect win (Rectangle 0 0 width height)
