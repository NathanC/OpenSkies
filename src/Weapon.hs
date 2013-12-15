module Weapon 
( Weapon(Weapon)
, WeaponType (HeatSeeking, Seeking, Straight, Explosive, Explosion)
, Missile (HeatSeekingMissile, StraightMissile, SeekingMissile, ExplosiveMissile)
, weaponX
, weaponY
, weaponRadius
, weaponTheta
, weaponSpeed
, typeOfWeapon
, weaponLifeSpan
, weaponAge
, isSeeking
, isExplosive
, isStraight
, isExplosion
, isHeatSeeking
, getExplosiveTimer
) where

import Data.Unique
import Targeting


-- fix: Standardize coordinates being touple or seperate. Touple cleaner usually? #style #ugly #records
data Weapon = Weapon {
                      weaponX :: Float
                     ,weaponY :: Float
                     ,weaponRadius :: Float
                     ,weaponTheta :: Float
                     ,weaponSpeed :: Float                                   
                     ,typeOfWeapon :: WeaponType
                     ,weaponLifeSpan :: Integer
                     ,weaponAge      :: Integer    
                    } deriving ( Eq)


--pattern matching for weapon type. Seems cleanest way to do it? Other way might be existentials.

isSeeking Weapon{typeOfWeapon = (Seeking _)} = True
isSeeking _ = False

isExplosive Weapon{typeOfWeapon =(Explosive _ )} = True
isExplosive _ = False

isStraight Weapon{typeOfWeapon = Straight} = True
isStraight _ = False

isExplosion Weapon{typeOfWeapon = Explosion} = True
isExplosion _ = False

isHeatSeeking Weapon{typeOfWeapon = HeatSeeking} = True
isHeatSeeking _ = False

getExplosiveTimer (Explosive t) = t




data WeaponType = HeatSeeking | Seeking (Maybe Unique) | Straight | Explosive Integer | Explosion
                    deriving ( Eq)


data Missile     = HeatSeekingMissile | StraightMissile | SeekingMissile | ExplosiveMissile deriving (Show, Enum, Bounded, Eq)


