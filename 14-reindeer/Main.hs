-- stack runghc --verbosity error --package parsec

import Text.Parsec
import Text.Parsec.String

type Seconds = Int
type Kilometers = Int
type KilometersPerSecond = Int

data Reindeer = Reindeer
  { _name :: String
  , _speed :: KilometersPerSecond
  , _stamina :: Seconds
  , _fatigue :: Seconds
  }
  deriving (Show, Eq)

nat :: (Read a, Num a) => Parser a
nat = read <$> many1 digit

pReindeer :: Parser Reindeer
pReindeer = do
  name <- many1 letter
  string " can fly "
  speed <- nat
  string " km/s for "
  stamina <- nat
  string " seconds, but then must rest for "
  rest <- nat
  string " seconds."
  endOfLine
  return $ Reindeer name speed stamina rest

pReindeers :: Parser [Reindeer]
pReindeers = pReindeer `manyTill` eof

runFor :: Seconds -> Reindeer -> Kilometers
runFor s r = go 0 s (_speed r, _stamina r) (0, _fatigue r)
  where go z 0 _ _ = z
        go z s a b =
          let t = min s (snd a)
          in go (z + t * fst a) (s - t) b a

data RaceState = RaceState
  { _score :: Int
  , _distance :: Kilometers
  , _running :: (KilometersPerSecond, Seconds)
  , _rest :: (KilometersPerSecond, Seconds)
  , _run :: (KilometersPerSecond, Seconds)
  }
  deriving (Eq, Show)

start :: Reindeer -> RaceState
start r =
  let run = (_speed r, _stamina r)
  in RaceState 0 0 run (0, _fatigue r) run

awardPoints :: [RaceState] -> [RaceState]
awardPoints rss = map (awardPoint $ maximum $ map _distance rss) rss
  where awardPoint d rs | d == _distance rs = rs { _score = _score rs + 1 }
                        | otherwise = rs

advanceAll :: [RaceState] -> [RaceState]
advanceAll = map advance
  where advance (RaceState z d (spd, t) rest run)
          | t == 1 = RaceState z (d+spd) rest run rest
          | otherwise = RaceState z (d+spd) (spd, t-1) rest run

main :: IO ()
main = do
  input <- getContents
  let time = 2503
  either print (mapM_ print) $ do
    ds <- parse pReindeers "<stdin>" input
    return
      [ maximum $ map (runFor time) ds
      , maximum $ map _score $ iterate (awardPoints . advanceAll) (map start ds) !! time
      ]
