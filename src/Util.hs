module Util
(pred'
,succ'
,pointsWithinDistance
,distanceBetweenTwoPoints
,circlesTouching
,updateCoords) where

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
