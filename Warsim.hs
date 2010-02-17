import System.Random
import Control.Monad.Random

data Stats = Stats {
  statWeaponSkill       :: Int,
  statBallisticSkill    :: Int,
  statStrength          :: Int,
  statToughness         :: Int,
  statWillpower         :: Int,
  statIntelligence      :: Int,
  statArmory            :: Int,
  statLeadership        :: Int,
  statSaves             :: Int,
  statInvulnerableSaves :: Int
} deriving (Show)

data WeaponType = Heavy | RapidFire | Assault deriving (Show, Eq)

data Weapon = Weapon {
  weaponName         :: String,
  weaponType         :: WeaponType,
  weaponAttacks      :: Int,
  weaponStrength     :: Int,
  weaponArmorPen     :: Int,
  weaponRange        :: Int,
  weaponIsTwinLinked :: Bool
} deriving (Show)

data Model = Model {
  modelStats   :: Stats,
  modelWeapons :: [Weapon],
  modelName    :: String
} deriving (Show)

data Unit = Unit {
  unitModels  :: [Model],
  unitName    :: String
} deriving (Show)

class Named a where
  name :: a -> String

instance Named Unit where
  name = unitName

instance Named Weapon where
  name = weaponName

instance Named Model where
  name = modelName

rand :: (Random a, RandomGen g) => a -> a -> Rand g [a]
rand x y = getRandomRs (x, y)
 
die :: (RandomGen g) => Rand g Int
die = getRandomR (1,6)

dice :: (RandomGen g) => Int -> Rand g [Int]
dice n = sequence $ replicate n die

rollDie = evalRandIO die
rollDice n = evalRandIO $ dice n

--
-- Tau armory
--
railgun = Weapon "Railgun" Heavy 1 10 1 72 True
smartMissileSystem = Weapon "Smart Missile System" Heavy 4 5 5 24 False
plasmaRifle = Weapon "Plasma Rifle" RapidFire 2 6 2 24 True
burstCannon = Weapon "Burst Cannon" Assault 3 5 5 18 False
pulseRifle = Weapon "Pulse Rifle" RapidFire 2 5 5 30 False
