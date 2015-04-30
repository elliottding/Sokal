import Control.Monad.State.Lazy
import Data.Array
import Data.List.Split (splitOn)
import System.Random
import System.Environment

type ModelTuple = (String, [(Int, Int)])

type FastModel = Array Int ModelTuple

type RandState = State StdGen

main :: IO ()
main = do
  fastModel <- readFastModel "./sokal.model"
  gen <- getStdGen
  args <- getArgs
  let limit = read (args !! 0) :: Int
  let ws = evalState (runModel limit fastModel) gen
  putStrLn $ unwords ws

-- Read a FastModel from a file.
readFastModel :: FilePath -> IO FastModel
readFastModel path = do
  str <- readFile path
  let readLine x = read x :: ModelTuple
  let model = map readLine $ lines str
  let maxIndex = length model - 1
  return $ array (0, maxIndex) [(i, model !! i) | i <- [0..maxIndex]]

-- Select a random element from an Array.
arraySelect :: Array Int a -> RandState a
arraySelect array = do
  index <- state $ randomR $ bounds array
  return $ array ! index

-- Select the first tuple with a first element greater than the given random variate.
weightedSelect_ :: [(Int, Int)] -> Int -> Int -> Maybe Int
weightedSelect_ [] _ _ = Nothing
weightedSelect_ (x:xs) r c
  | total >= r = Just $ snd x
  | otherwise = weightedSelect_ xs r total
  where total = c + (fst x)

-- Weighted random selection of an ID from the list of frequency, ID pairs.
weightedSelect :: [(Int, Int)] -> RandState (Maybe Int)
weightedSelect xs = do
  r <- state $ randomR (1, sum (map fst xs))
  return $ weightedSelect_ xs r 0

-- Select a random next ModelTuple from a current ModelTuple and the given FastModel.
selectNext :: ModelTuple -> FastModel -> RandState (Maybe ModelTuple)
selectNext (word, successors) fm = do
  nextId <- weightedSelect successors
  case nextId of
    Nothing -> return Nothing
    Just i -> return $ Just $ fm ! i

-- Select a random ModelTuple that starts a sentence.
getStart :: FastModel -> RandState ModelTuple
getStart fm = do
  initial <- arraySelect fm
  getStart_ initial fm

-- getStart recursion function that keeps track of the current ModelTuple.
getStart_ :: ModelTuple -> FastModel -> RandState ModelTuple
getStart_ current@(currentWord, _) fm = do
  maybeNext <- selectNext current fm
  case maybeNext of
    -- If we have no more successors, try again from the start.
    Nothing -> getStart fm
    Just next ->
      -- If the current word ends in a period, then this next word is the start of a sentence.
      if last currentWord == '.'
        then return next
        else getStart_ next fm

-- Run the FastModel up to the specified word limit.
runModel :: Int -> FastModel -> RandState [String]
runModel limit fm = do
  start <- getStart fm
  -- Iterate until the word limit is reached.
  iter 0 start where
    iter count current@(word, successors) = do
      if (last word == '.') && (count >= limit)
        then return [word]
        else do
          maybeNext <- selectNext current fm
          case maybeNext of
            Nothing -> return [word]
            Just next -> do
              ws <- iter (count + 1) next
              return (word:ws)
