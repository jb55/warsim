data Stats = {
  weaponSkill       :: Int,
  ballisticSkill    :: Int,
  strength          :: Int,
  toughness         :: Int,
  willpower         :: Int,
  intelligence      :: Int,
  armory            :: Int,
  leadership        :: Int,
  saves             :: Int,
  invulnerableSaves :: Int
}

data WeaponType = Heavy | RapidFire | Assault 
                  deriving (Show, Eq, Bounded)

data Weapon = {
  name              :: String,
  weaponType        :: WeaponType,
  numAttacks        :: Int,
  strength          :: Int,
  armorPen          :: Int,
  range             :: Int,
  twinLinked        :: Bool
}

data Model = {
  stats :: Stats,
  weapons :: [Weapon]
} deriving (Show)

data Unit = {
  models :: [Model],
  name :: String
} deriving (Show)

defaultRange = 72

--
-- Tau armory
--
railgun = Weapon "Railgun" Heavy 1 10 1 defRange True
smartMissileSystem = Weapon "Smart Missile System" Heavy 4 5 5 24 False
plasmaRifle = Weapon "Plasma Rifle" RapidFire 2 6 2 24 True
burstCannon = Weapon "Burst Cannon" Assault 3 5 5 18 False
pulseRifle = Weapon "Pulse Rifle" RapidFire 2 5 5 30 False


