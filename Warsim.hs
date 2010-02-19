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

type StatType = Int

type BallisticSkill = StatType
type Strength = StatType
type Toughness = StatType

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

data Result = Result {
  resultWounds       :: Probability,
  resultHits         :: Probability,
  resultSaves        :: Probability,
  resultKills        :: Probability
}

data Model = Model {
  modelWeapons      :: [Weapon],
  modelName         :: String,
  weaponSkill       :: Int,
  ballisticSkill    :: BallisticSkill,
  strength          :: Strength,
  toughness         :: Toughness,
  willpower         :: Int,
  intelligence      :: Int,
  armory            :: Int,
  leadership        :: Int,
  saves             :: Int,
  invulnerableSaves :: Int
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
 
d6Prob :: Int -> Probability
d6Prob toHit = (>= toHit) ?? d6

limit :: (Ord a) => a -> a -> a
limit a b
  | b > a     = a
  | otherwise = b

hits :: Weapon -> BallisticSkill -> Probability
hits w bs = 
  limit attacks $ (totalHitProb + totalRerollProb) - overlap
  where 
    attacks         = toRational $ weaponAttacks w
    toHit           = 7 - bs
    hitProb         = d6Prob toHit
    numMissed       = toRational bs
    rerolledHitProb = if weaponIsTwinLinked w then hitProb else 0
    totalHitProb    = hitProb * attacks
    totalRerollProb = rerolledHitProb * numMissed
    overlap         = totalHitProb * totalRerollProb


-- Toughness: toughness of defending model
-- Strength: strength of attacking model
-- Probability: hit probability
wounds :: Toughness -> Strength -> Probability -> Probability
wounds t s p = (d6Prob $ toWound t s) * p

battleModels' :: Model -> Model -> Probability
battleModels' a d = battleModels a d (head $ modelWeapons a)

battleModels :: Model -> Model -> Weapon -> Probability
battleModels a d w = wp
  where
    t = toughness d
    s = strength a
    hp = hits w (ballisticSkill a)
    wp = wounds t s hp

toWound :: Toughness -> Strength -> Int
toWound t s
  | val < 2 = 2
  | val == 7 = 6
  | val == 8 = 7
  | otherwise = val
  where val = (t - s) + 4

invSave = 7

--
-- Tau armory
--
railgun = Weapon "Railgun" Heavy 1 10 1 72 True
smartMissileSystem = Weapon "Smart Missile System" Heavy 4 5 5 24 False
plasmaRifle = Weapon "Plasma Rifle" RapidFire 2 6 2 24 True
burstCannon = Weapon "Burst Cannon" Assault 3 5 5 18 False
pulseRifle = Weapon "Pulse Rifle" RapidFire 2 5 5 30 False

tauWeapons = [railgun, smartMissileSystem, plasmaRifle, burstCannon, pulseRifle]

fireWarrior = Model [pulseRifle] "Fire Warrior" 2 3 3 3 1 2 1 7 4 7 
