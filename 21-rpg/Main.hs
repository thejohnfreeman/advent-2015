-- stack runghc --verbosity error

import qualified Data.List as L
import Data.Ord (comparing)

interleave :: [[a]] -> [a]
interleave = L.concat . L.transpose

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = interleave [xss, map (x:) xss] where xss = powerset xs

data Gear = Gear
  { _pieces :: [String]
  , _cost :: Int
  , _damage :: Int
  , _armor :: Int
  }
  deriving Show

weapons =
  [ Gear ["dagger"]      8 4 0
  , Gear ["shortsword"] 10 5 0
  , Gear ["warhammer"]  25 6 0
  , Gear ["longsword"]  40 7 0
  , Gear ["greataxe"]   74 8 0
  ]

armors =
  [ Gear ["leather"]    13 0 1
  , Gear ["chainmail"]  31 0 2
  , Gear ["splintmail"] 53 0 3
  , Gear ["bandedmail"] 75 0 4
  , Gear ["platemail"]  102 0 5
  ]

rings =
  [ Gear ["damage +1"]   25 1 0
  , Gear ["damage +2"]   50 2 0
  , Gear ["damage +3"]  100 3 0
  , Gear ["defense +1"]  20 0 1
  , Gear ["defense +2"]  40 0 2
  , Gear ["defense +3"]  80 0 3
  ]

addGear (Gear ns1 c1 d1 a1) (Gear ns2 c2 d2 a2) = Gear (ns1++ns2) (c1+c2) (d1+d2) (a1+a2)
costsLessThan (Gear _ c1 _ _) (Gear _ c2 _ _) = c1 < c2

gearCombos :: [Gear]
gearCombos = do
  w <- weapons
  a <- armors
  rs <- filter ((<=2) . length) $ powerset rings
  map (L.foldl1' addGear) [w:rs, w:a:rs]

data CharacterType = Player | Boss

data Character = Character
  { _type :: CharacterType
  , _hp :: Int
  , _gear :: Gear
  }

boss = Character
  { _type = Boss
  , _hp = 100
  , _gear = Gear { _pieces = [], _cost = 0, _damage = 8, _armor = 2 }
  }

newPlayer gear = Character { _type = Player, _hp = 100, _gear = gear }

attacks :: Character -> Character -> Character
p1 `attacks` p2 =
  let dmg = max 1 $ (_damage . _gear) p1 - (_armor . _gear) p2
  in p2 { _hp = _hp p2 - dmg }

type GameState = (Character, Character)

startGame :: Character -> GameState
startGame player = (player, boss)

isGameOver :: GameState -> Bool
isGameOver (p, _) = _hp p <= 0

takeTurn :: GameState -> GameState
takeTurn (s, t) = (s `attacks` t, s)

gearWins :: Gear -> Bool
gearWins gear =
  let (l, w) = until isGameOver takeTurn (startGame $ newPlayer gear)
  in case _type w of
    Player -> True
    Boss -> False

main :: IO ()
main = do
  print $ L.minimumBy (comparing _cost) $ filter gearWins gearCombos
  print $ L.maximumBy (comparing _cost) $ filter (not . gearWins) gearCombos
