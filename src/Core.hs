{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Core (day9) where

import Data.Char (ord)
import Data.List (nub, scanl', sort, tails, transpose)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Tree (Tree (Node))
import Data.Void (Void)
import Text.Megaparsec (
    Parsec,
    many,
    manyTill,
    parseMaybe,
    sepBy,
    sepBy1,
    sepEndBy,
    (<|>),
 )
import Text.Megaparsec.Char (char, string)
import qualified Text.Megaparsec.Char as L
import qualified Text.Megaparsec.Char.Lexer as L

day1 :: IO (Int, Int)
day1 = do
    input1 <- getDayInput "1"
    let elves = T.splitOn "\n\n" $ T.strip input1
        elves' = mapMaybe (parseMaybe pElf) elves
        totalCals = sumElf <$> elves'
    pure $ (maximum totalCals, sum . take 3 . reverse $ sort totalCals) -- day 1 part 1

mylength :: Num p => [a] -> p
mylength [] = 0
mylength (_ : as) = 1 + mylength as

getDayInput :: String -> IO Text
getDayInput dayNumber = do
    rf <- TIO.readFile ("day" <> dayNumber <> ".txt")
    pure $ T.strip rf

getDayInput' :: String -> IO String
getDayInput' dayNumber = do
    readFile ("day" <> dayNumber <> ".txt")

getTestInput :: String -> IO Text
getTestInput dayNumber = TIO.readFile ("test" <> dayNumber <> ".txt")

getTestInput' :: String -> IO String
getTestInput' dayNumber = readFile ("test" <> dayNumber <> ".txt")

type Parser = Parsec Void Text

data Elf = Elf {calories :: [Int]} deriving (Eq, Ord, Show)

sumElf :: Elf -> Int
sumElf (Elf cs) = sum cs

pElf :: Parser Elf
pElf = Elf <$> sepBy1 L.decimal (char '\n')

-- day 2

day2 :: IO ()
day2 = do
    -- input2 <- getTestInput "2"
    input2 <- getDayInput "2"
    let rounds = mapMaybe (parseMaybe (pRPS `sepBy` string " ")) (T.lines $ T.strip input2)
    print (sum $ scoreRound <$> rounds, sum $ scoreRound' <$> rounds)

data RPS
    = A -- Rock
    | B -- Paper
    | C -- Scissor
    | X -- Rock
    | Y -- Paper
    | Z -- Scissor
    deriving (Eq, Show)

scoreRound :: [RPS] -> Int
scoreRound [x, y] = rpsVal y + if x < y then 6 else if x > y then 0 else 3
scoreRound nope = error $ "scoreRound got bad input " <> show nope

pRPS :: Parser RPS
pRPS =
    A <$ string "A"
        <|> B <$ string "B"
        <|> C <$ string "C"
        <|> X <$ string "X"
        <|> Y <$ string "Y"
        <|> Z <$ string "Z"

data RPSGame = RPSGame
    { first :: RPS
    , second :: RPS
    }

rpsVal :: RPS -> Int
rpsVal =
    \case
        A -> 1
        B -> 2
        C -> 3
        X -> 1
        Y -> 2
        Z -> 3

nextRPS :: Int -> Int
nextRPS 1 = 2
nextRPS 2 = 3
nextRPS 3 = 1
nextRPS _ = error "no"

prevRPS :: (Eq a, Num a, Num p) => a -> p
prevRPS 1 = 3
prevRPS 2 = 1
prevRPS 3 = 2
prevRPS _ = error "plz no"

instance Ord RPS where
    -- Rock beats Scissor
    -- compare A C = GT
    compare A Z = GT
    compare A X = EQ
    compare A Y = LT
    -- Paper beats Rock
    -- compare B A = GT
    compare B X = GT
    compare B Y = EQ
    compare B Z = LT
    -- Scissors beats Paper
    compare C X = LT
    compare C Z = EQ
    compare C Y = GT
    compare _ _ = LT

{-
X lose means play the lower value and add 0
Y draw means play the same value and add 3
Z win means play the higher value add 6
-}
scoreRound' :: [RPS] -> Int
scoreRound' [input, X] = 0 + prevRPS (rpsVal input)
scoreRound' [input, Y] = 3 + rpsVal input
scoreRound' [input, Z] = 6 + nextRPS (rpsVal input)
scoreRound' _ = error "nope"

-- day3
day3 :: IO ()
day3 = do
    vals <- getDayInput' "3"
    let sacks = lines vals
    -- print $ foo <$> sacks
    print $ sum $ (itemVal . Set.toList . foo <$> sacks)
    print $ sum (itemVal . getCommon <$> threeLines sacks)

foo :: Ord a => [a] -> Set.Set a
foo sack =
    let compartmentSize = length sack `div` 2
     in Set.fromList (take compartmentSize sack) `Set.intersection` Set.fromList (drop compartmentSize sack)

itemVal :: String -> Int
itemVal [c] =
    if letterVal >= 97 -- lowercase
        then letterVal - 96
        else letterVal - 38
  where
    letterVal = ord c
itemVal _ = error "bad input"

threeLines :: [String] -> [(String, String, String)]
threeLines [] = []
threeLines (a : b : c : rest) = [(a, b, c)] <> threeLines rest
threeLines oops = error $ show oops

getCommon :: Ord a => ([a], [a], [a]) -> [a]
getCommon (a, b, c) = common
  where
    common = Set.toList $ Set.fromList a `Set.intersection` Set.fromList b `Set.intersection` Set.fromList c

-- day4
day4 :: IO ()
day4 = do
    ranges <- getDayInput "4"
    let whatevs = mapMaybe (parseMaybe pRange) (T.lines ranges)
    print $ length $ filter id $ wrap contains <$> whatevs
    print $ length $ filter id $ wrap contains' <$> whatevs

wrap :: ((a1, b1) -> (a2, b2) -> t) -> (a1, b1, a2, b2) -> t
wrap f (a, b, c, d) = f (a, b) (c, d)

contains :: (Enum a, Eq a) => (a, a) -> (a, a) -> Bool
contains (a, b) (y, z) =
    length (nub $ intervalLeft <> intervalRight) == max (length intervalLeft) (length intervalRight)
  where
    intervalLeft = enumFromTo a b
    intervalRight = enumFromTo y z

contains' :: (Enum a, Eq a) => (a, a) -> (a, a) -> Bool
contains' (a, b) (y, z) =
    length (nub $ intervalLeft <> intervalRight) < length intervalLeft + length intervalRight
  where
    intervalLeft = enumFromTo a b
    intervalRight = enumFromTo y z

pRange :: Parser (Int, Int, Int, Int)
pRange = do
    (,,,) <$> L.decimal <* char '-' <*> L.decimal <* char ',' <*> L.decimal <* char '-' <*> L.decimal

-- day5

day5 :: IO ()
day5 = do
    poop <- getDayInput "5"
    let [raw_stacks, instructions] = T.splitOn "\n\n" poop
    let stacks = getStacks raw_stacks
        moves = mapMaybe (parseMaybe pMove) (T.lines instructions)
    print $ "day 5 part 1 " <> (head <$> moveStacks stacks moves)
    print $ "day 5 part 2 " <> (head <$> moveStacks' stacks moves)

getStacks :: Text -> [String]
getStacks crap = T.unpack <$> only_stacks (important_lines crap)
  where
    important_lines = T.transpose . init . T.lines
    only_stacks stuff = filter (not . T.null) (T.filter nobrackets . T.strip <$> stuff)

nobrackets :: Char -> Bool
nobrackets '[' = False
nobrackets ']' = False
nobrackets _ = True

data Move = Move
    { howMany :: Int
    , fromWhere :: Int
    , toWhere :: Int
    }
    deriving (Eq, Ord, Show)
pMove :: Parser Move
pMove = Move <$ string "move " <*> L.decimal <* string " from " <*> L.decimal <* string " to " <*> L.decimal

moveStacks :: [String] -> [Move] -> [String]
moveStacks done [] = done
moveStacks stacks (Move mcount origin destination : ms) =
    let (o, d) = moveFromLeft (stacks !! (origin - 1)) (stacks !! (destination - 1)) mcount
     in moveStacks (replace (replace stacks (origin - 1) o) (destination - 1) d) ms

moveFromLeft :: String -> String -> Int -> (String, String)
moveFromLeft origin destination 0 = (origin, destination)
moveFromLeft (a : as) destination n = moveFromLeft as (a : destination) (n - 1)
moveFromLeft _ _ _ = error "moveFromLeft failed"

moveStacks' :: [String] -> [Move] -> [String]
moveStacks' done [] = done
moveStacks' stacks (Move mcount origin destination : ms) =
    let (o, d) = moveAll (stacks !! (origin - 1)) (stacks !! (destination - 1)) mcount
     in moveStacks' (replace (replace stacks (origin - 1) o) (destination - 1) d) ms

moveAll :: String -> String -> Int -> (String, String)
moveAll origin destination 0 = (origin, destination)
moveAll origin destination n = (drop n origin, take n origin <> destination)

replace :: [a] -> Int -> a -> [a]
replace (_ : as) 0 new = new : as
replace (a : as) n new = a : replace as (n - 1) new
replace _ _ _ = error "replace got bad input"

-- day 6
day6 :: IO ()
day6 = do
    puz <- getDayInput' "6"
    let chunks n = nub . take n <$> tails puz
        countUniqs n = n + length (takeWhile (\x -> length x < n) $ chunks n)
    print $ (countUniqs 4, countUniqs 14) -- 4 is the length of the 'key', then 14

-- day 7
day7 :: IO ()
day7 = do
    cmds <- getTestInput "7"
    -- let cmd_lines = lines cmds
    print $ parseMaybe (many pFileOp) cmds

data File = F Int String | Dir String deriving (Eq, Ord, Show)

data FileOp
    = CD String
    | LS [File]
    deriving (Eq, Ord, Show)

pFile :: Parser File
pFile =
    do
        Dir <$ string "dir " <*> manyTill L.asciiChar (string "\n")
        <|> F <$> L.decimal <* string " " <*> manyTill L.asciiChar (string "\n")

pFileOp :: Parser FileOp
pFileOp = do
    CD <$ string "$ cd " <*> manyTill L.asciiChar (char '\n')
        <|> LS <$ string "$ ls\n" <*> many pFile

-- parsing is done? now to build the tree
type FileSystem = Tree (String, [File]) -- String holds the name of this directory

ourFS :: FileSystem
ourFS =
    Node
        ("/", [F 0 "b.txt", F 0 "c.dat", Dir "a"])
        [ Node ("a", [F 0 "f", F 0 "g", F 0 "h.lst"]) [Node ("e", []) []]
        , Node ("d", []) []
        ]

-- upDir cursor =
type Cursor = String

-- startFSTree ((CD "/") : ops) = buildFSTree ops (Node [Dir "/"] []) "/"
-- startFSTree _ = error "didn't get root node first"

-- buildFSTree :: [FileOp] -> FileSystem -> Cursor -> FileSystem
-- buildFSTree [] fs c = fs
-- buildFSTree ((CD "..") : fos) fs c = _w6sq
-- buildFSTree ((CD dir) : fos) fs c = buildFSTree fos fs
-- buildFSTree ((LS []) : fos) fs c = buildFSTree fos fs c -- no more files to add to tree
-- buildFSTree ((LS (fi : fis)) : fos) fs c = _w6st --

{-
root is cd /
parent is cd ..
What's "cd foo" ?
I guess it has to be walk the children until we find a subdir called "foo"
-}

-- day 8
{- iterate takeWhile less than? -}

-- day8 :: IO ()
day8 :: IO Int
day8 = do
    tt <- getDayInput' "8"
    let treeAndCoords = treesWithLocation tt
    pure $ length $ nub $ concat $ concat $ [leftSide treeAndCoords, rightSide treeAndCoords, topSide treeAndCoords, bottomSide treeAndCoords]
  where
    leftSide = fmap visible
    rightSide = fmap (visible . reverse)
    topSide tac = visible <$> transpose tac
    bottomSide tac = visible . reverse <$> transpose tac

-- this duplicates the outermost tree if the next tree is also visible, hm
visible :: Ord a => [(a, b)] -> [(a, b)]
visible allts@(a : _) = a : visibleMax allts (fst a)
visible _ = error "visible got bad input"

visible' :: Ord a => [(a, b)] -> [(a, b)]
visible' [] = []
visible' [_] = []
visible' (a : b : cs) = if fst a < fst b then b : visible' (b : cs) else visible' $ b : cs

visibleMax :: Ord t => [(t, b)] -> t -> [(t, b)]
visibleMax [] _ = []
visibleMax [_] _ = []
visibleMax (_ : b : cs) n = if n < fst b then b : visibleMax (b : cs) (fst b) else visibleMax (b : cs) n

annoX :: (Num a, Enum a) => [[b]] -> [[(a, b)]]
annoX rows = [zip [1 ..] this_row | this_row <- rows]

annoY :: [(a, b)] -> t -> [((a, t), b)]
annoY [] _ = []
annoY ((x, c) : more) n = ((x, n), c) : annoY more n

annotate :: (Num a, Num t, Enum a, Enum t) => [[b]] -> [[((a, t), b)]]
annotate rows = zipWith annoY (annoX rows) [1 ..]

swappy :: (b, a) -> (a, b)
swappy (a, b) = (b, a)

treesWithLocation :: String -> [[(Char, (Int, Int))]]
treesWithLocation all_lines = fmap (fmap swappy) (annotate $ lines all_lines)

-- day 9
day9 :: IO ()
day9 = do
    dt <- getDayInput "9"
    let exDirs = parseCleanDay9 dt
    putStrLn $ "day 1 part 1 " <> (show $ evalDay9 exDirs (scanl' tailFollow (0, 0)))
    putStrLn $ "day 1 part 2 " <> (show $ evalDay9 exDirs (oneRope . oneRope . oneRope . oneRope . oneRope . oneRope . oneRope . oneRope . oneRope))
  where
    oneRope = scanl1 tailFollow

parseCleanDay9 :: Text -> [Direction]
parseCleanDay9 txt =
    let directions = parseMaybe (pDirection `sepEndBy` char '\n') txt
     in concat $ maybe [] (fmap expandDirections) directions

evalDay9 :: Eq a => [Direction] -> ([(Int, Int)] -> [a]) -> Int
evalDay9 expandedDirs tailF =
    let headLocations = scanl' stepHead (0, 0) expandedDirs
        tailLocations = tailF headLocations
     in length $ nub $ tailLocations

data Direction = R Int | L Int | U Int | D Int deriving (Eq, Ord, Show)

pDirection :: Parser Direction
pDirection =
    R <$ string "R " <*> L.decimal
        <|> L <$ string "L " <*> L.decimal
        <|> U <$ string "U " <*> L.decimal
        <|> D <$ string "D " <*> L.decimal

expandDirections :: Direction -> [Direction]
expandDirections (D n) = replicate n (D 1)
expandDirections (L n) = replicate n (L 1)
expandDirections (R n) = replicate n (R 1)
expandDirections (U n) = replicate n (U 1)

stepHead :: (Int, Int) -> Direction -> (Int, Int)
stepHead (x, y) (D 1) = (x, y -1)
stepHead (x, y) (U 1) = (x, y + 1)
stepHead (x, y) (L 1) = (x -1, y)
stepHead (x, y) (R 1) = (x + 1, y)
stepHead _ _ = error "stepHead got bad input"

{- follow function
If the head is ever two steps directly up, down, left, or right from the tail, the tail must also move one step in that direction so it remains close enough

Otherwise, if the head and tail aren't touching and aren't in the same row or column, the tail always moves one step diagonally to keep up
-}

tailFollow :: (Int, Int) -> (Int, Int) -> (Int, Int)
tailFollow t@(xt, yt) h@(xh, yh) =
    if
            | isTouching h t -> t
            | yh == yt -> if xh > xt then (xt + 1, yt) else (xt - 1, yt) -- same row
            | xh == xt -> if yh > yt then (xt, yt + 1) else (xt, yt - 1) -- same column
            | otherwise -> diagonalTail h t

diagonalTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
diagonalTail (xh, yh) (xt, yt) =
    if
            | xh > xt ->
                if
                        | yh > yt -> (xt + 1, yt + 1)
                        | otherwise -> (xt + 1, yt -1)
            | otherwise ->
                if
                        | yh > yt -> (xt -1, yt + 1)
                        | otherwise -> (xt -1, yt -1)

isTouching :: (Int, Int) -> (Int, Int) -> Bool
isTouching (x1, y1) (x2, y2) = abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1
