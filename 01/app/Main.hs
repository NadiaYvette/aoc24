module Main (main) where

import Control.Applicative (Alternative (..))
import Control.Arrow ((***))
import Control.Monad (join)
import Data.Functor.Syntax ((<$$>))
import Data.List (sort)
import Data.Maybe (mapMaybe)

import "multiset" Data.MultiSet (MultiSet)
import qualified "multiset" Data.MultiSet as
  MultiSet (fromList, occur, toOccurList)

import "optparse-applicative" Options.Applicative ((<**>))
import qualified "optparse-applicative" Options.Applicative as
  Options (InfoMod, Parser, ParserInfo, execParser, flag', fullDesc
          , header, help, helper, info, long, metavar, progDesc, short
          , strOption)

import "text" Data.Text (Text)
import qualified "text" Data.Text as
  Text (lines, words)
import qualified "text" Data.Text.IO as
  Text (getContents, readFile)
import qualified "text" Data.Text.Read as
  Text (decimal)


data Input
  = FileInput FilePath
  | StdInput
  deriving (Eq, Ord, Read, Show)

fileInput :: Options.Parser Input
fileInput = fmap FileInput . Options.strOption
  $  Options.long "file"
  <> Options.short 'f'
  <> Options.metavar "FILENAME"
  <> Options.help "Input file"

stdInput :: Options.Parser Input
stdInput = Options.flag' StdInput
  $  Options.long "stdin"
  <> Options.help "Read from stdin"

inputOpts :: Options.Parser Input
inputOpts = fileInput <|> stdInput

desc :: Options.InfoMod Input
desc = Options.fullDesc
    <> Options.progDesc "sum distances between n-th positions"
    <> Options.header "aoc2401 - Advent Of Code 2024 problem 1"

optParser :: Options.ParserInfo Input
optParser = Options.info (inputOpts <**> Options.helper) desc

getAOCcontents :: Input -> IO Text
getAOCcontents = \case
  FileInput f -> Text.readFile f
  StdInput -> Text.getContents

parseAOCvals :: Text -> [[Either String (Integer, Text)]]
parseAOCvals txt = Text.decimal <$$> Text.words <$> Text.lines txt

getIntPairs :: [Either String (Integer, Text)] -> Maybe (Integer, Integer)
getIntPairs = \case
  [Right (x, ""), Right (y, "")] -> Just (x, y)
  _ -> Nothing

dup :: (t -> t') -> (t, t) -> (t', t')
dup = join (***)

-- sumDists :: [(Integer, Integer)] -> Integer
-- sumDists = sum . uncurry (zipWith \x y -> abs (x - y)) .  dup sort . unzip

main :: IO ()
main = do
  txt <- getAOCcontents =<< Options.execParser optParser
  let as, bs :: [Integer]
      (as, bs) = dup sort . unzip . mapMaybe getIntPairs $ parseAOCvals txt
      ams, bms :: MultiSet Integer
      (ams, bms) = dup MultiSet.fromList (as, bs)
  print . sum $ zipWith (\x y -> abs (x - y)) as bs
  print $ sum [a * fromIntegral (o * (a `MultiSet.occur` bms))| (a, o) <- MultiSet.toOccurList ams]
