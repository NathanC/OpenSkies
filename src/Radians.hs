module Radians 
(combineAngles
,reduceAngle
,angleBetweenTwoPoints
,shiftTowards
,distanceBetweenAngles
,radiansToDegrees
)where



radiansToDegrees t = 180/pi * t



-- |Takes any angle, in radians, and reduces it to the coterminal
--  angle in the range [-pi,pi]

reduceAngle theta
      | rawReducedAngle <= pi = rawReducedAngle
      | otherwise = -2*pi + rawReducedAngle

      where rawReducedAngle 
              | theta >= 0 && theta <= 2*pi = theta
              | theta < 0 = reduceAngle (theta + 2*pi)
              | otherwise = reduceAngle (theta - 2*pi)



-- |Gives a new angle in the range [0,2*pi], given an old angle and a change in radians  

combineAngles theta deltaTheta = reduceAngle (theta+deltaTheta)





-- |Gives the angle, in radians, between two points.
--  i.e., the angle that point a would have to "travel" at,
--  in respect to the positive x axis, to reach point b
--  Resulting angle is bounded by [-pi,pi]
angleBetweenTwoPoints (x1,y1) (x2,y2) 
        =  atan2 deltaYraw deltaXraw

         where deltaXraw = x2-x1
               deltaYraw = y2-y1
               deltaX = if abs deltaXraw < 0.001 then 0 else deltaXraw
               deltaY = if abs deltaYraw < 0.001 then 0 else deltaYraw
               rawTheta = atan2 deltaY deltaX  
               
      


-- |Takes a starting angle, target angle, and shifts the starting
-- angle to the target angle by a given percentage of the archlength 
-- (can be thought of of the how fast the angle is turning towards it)

shiftTowards currentAngle targetAngle percent = combineAngles currentAngle (distance * percent)                        
    
      where distance = distanceBetweenAngles currentAngle targetAngle 


       

-- |Returns the shortest angle, in radians, that descripes the arc between two points
--  on a circle. Result will be in range [-pi,pi]

distanceBetweenAngles angle1 angle2
    | abs(diff) > pi = (-2*pi) + diff 
    | otherwise = diff 
    where diff = reduceAngle $ angle2-angle1
