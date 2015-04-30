import Data.List (group, sort, intercalate)
import Data.List.Split (splitOn)
import Data.Text (strip)
import Data.Maybe (fromJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Text.HTML.TagSoup

type PrimitiveModel a = Map (a, a) [a]

type ProcessedModel a = [(a, [(Int, Int)])]

main :: IO()
main = do
  contents <- getContents
  let urls = filter (/= "") $ splitOn "\n" contents
  let foldf url m = putStrLn ("Reading " ++ url) >> updatePrimitiveModelFromUrlIO url m
  primitiveModel <- foldr foldf (return Map.empty) urls
  let processedModel = makeProcessedModel primitiveModel
  writeProcessedModel "./sokal.model" processedModel
  putStrLn "Successfully wrote model to sokal.model"

-- Read all HTML from the provided web URL.
readUrl :: String -> IO String
readUrl url = simpleHTTP (getRequest url) >>= getResponseBody

-- Extract the body text from an HTML string and split it into a list of words.
harvest :: String -> [String]
harvest = filter (/= "") . concatMap (splitOn " ") . parseHtml where
  -- Attempt to drop unnecessary text by matching to body and bio div sections, if they exist
  dropHead = dropWhile (/= TagOpen "div" [("id", "body")])
  dropTail = takeWhile (/= TagOpen "div" [("class", "bio")])
  -- Partition to <p> tags
  createPartitions = partitions (== TagOpen "p" [])
  -- Extract all text between <p> tags
  extractText = map (innerText . takeWhile (/= TagClose "p"))
  parseHtml = extractText . createPartitions . dropTail . dropHead . parseTags

-- Create a list of in-order triples from a list.
makeTriples :: [a] -> [(a, a, a)]
makeTriples xs@(x:y:ys)
  | length xs < 3 = []
  | otherwise = (x, y, head ys) : makeTriples (y:ys)

-- Create a PrimitiveModel from a list.
makePrimitiveModel :: (Eq a, Ord a) => [a] -> PrimitiveModel a
makePrimitiveModel = updatePrimitiveModel Map.empty

-- Update a PrimitiveModel with a given list.
updatePrimitiveModel :: (Eq a, Ord a) => PrimitiveModel a -> [a] -> PrimitiveModel a
updatePrimitiveModel m = foldr combine m . makeTriples where
  combine (x, y, v) = Map.alter insertf (x, y) where
    insertf Nothing = Just [v]
    insertf (Just vs) = Just (v:vs)

-- Update a PrimitiveModel using the parsed text loaded from the provided url.
updatePrimitiveModelFromUrl :: PrimitiveModel String -> String -> IO (PrimitiveModel String)
updatePrimitiveModelFromUrl m url = readUrl url >>= return . updatePrimitiveModel m . harvest

-- IO wrapped version of updatePrimitiveModelFromUrl. Convenience function for use with foldr.
updatePrimitiveModelFromUrlIO :: String -> IO (PrimitiveModel String) -> IO (PrimitiveModel String)
updatePrimitiveModelFromUrlIO = flip (>>=) . flip updatePrimitiveModelFromUrl

-- Returns a list of frequency, element pairs sorted in descending order by frequency.
elementFrequency :: Ord a => [a] -> [(Int, a)]
elementFrequency = reverse . sort . map (\x -> (length x, head x)) . group . sort

-- Convert lists of successor words into frequency lists.
convertToFrequencies :: Ord a => PrimitiveModel a -> Map (a, a) [(Int, a)]
convertToFrequencies = fmap elementFrequency

-- Assign unique IDs to each key tuple in the PrimitiveModel.
assignIDs :: Ord a => PrimitiveModel a -> Map (a, a) Int
assignIDs = Map.fromList . zipWith (\x (k, v) -> (k, x)) [0..] . Map.toList

-- Create a ProcessedModel from a PrimitiveModel.
makeProcessedModel :: Ord a => PrimitiveModel a -> ProcessedModel a
makeProcessedModel m = map (processElement (assignIDs m)) $ Map.toList $ convertToFrequencies m where
  makeFreqIDList _ [] _ = []
  makeFreqIDList y (c:cs) ids = case Map.lookup (y, snd c) ids of
    Just i -> (fst c, i) : (makeFreqIDList y cs ids)
    Nothing -> makeFreqIDList y cs ids
  processElement ids ((x, y), cs) = (y, makeFreqIDList y cs ids)

-- Write the ProcessedModel to a file.
writeProcessedModel :: FilePath -> ProcessedModel String -> IO()
writeProcessedModel path m = do
  let str = intercalate "\n" $ map show m
  writeFile path str
