import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution ((??),(?=<<),)
import Control.Monad

type Die = Int
type Probability = Rational
type Dist = Dist.T Probability

die :: Die -> Dist Die
die n = Dist.uniform [1..n]

d6 :: Dist Die
d6 = die 6

type BallisticSkill = Int

data Stats = Stats {
  weaponSkill       :: Int,
  ballisticSkill    :: BallisticSkill,
  strength          :: Int,
  toughness         :: Int,
  willpower         :: Int,
  intelligence      :: Int,
  armory            :: Int,
  leadership        :: Int,
  saves             :: Int,
  invulnerableSaves :: Int
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
 
limit :: (Ord a) => a -> a -> a
limit a b
  | b > a     = a
  | otherwise = b


hits :: Weapon -> BallisticSkill -> Probability
hits w bs = limit attacks $ (hitProb * attacks) + rerolledMisses
  where attacks = toRational $ weaponAttacks w
        toHit = 7 - bs
        numMissed = toRational bs
        hitProb = (< toHit) ?? d6
        rerolledHitProb = if weaponIsTwinLinked w then hitProb else 0
        rerolledMisses = rerolledHitProb * numMissed

wounds :: Model -> Model -> Int
wounds = undefined

invSave = 7

--
-- Tau armory
--
railgun = Weapon "Railgun" Heavy 1 10 1 72 True
smartMissileSystem = Weapon "Smart Missile System" Heavy 4 5 5 24 False
plasmaRifle = Weapon "Plasma Rifle" RapidFire 2 6 2 24 True
burstCannon = Weapon "Burst Cannon" Assault 3 5 5 18 False
pulseRifle = Weapon "Pulse Rifle" RapidFire 2 5 5 30 False

fireWarrior = Model (Stats 2 3 3 3 1 2 1 7 4 7) [pulseRifle] "Fire Warrior"
