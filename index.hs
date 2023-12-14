import Text.Read (readMaybe)

-- Types
type Board = [Int]

-- Output
putRow :: Int -> Int -> IO ()
putRow row count = do
  if count == 0
    then putStr ""
    else do
      putStrLn (show row ++ ": " ++ replicate count '*')

putBoardRow :: Board -> Int -> IO ()
putBoardRow board row = do
  if null board
    then putStr ""
    else do
      putRow row (head board)
      putBoardRow (tail board) (row + 1)

putBoard :: Board -> IO ()
putBoard board = do
  if null board
    then putStr ""
    else do
      putStrLn "----------NIM----------"
      putBoardRow board 1
      putStrLn "-----------------------"

-- Input
getNatNum :: IO Int
getNatNum = do
  str <- do getLine
  case readMaybe str of
    Just n -> return n
    Nothing -> return (-1)

validRow :: Board -> Int -> Bool
validRow board row = row > 0 && row < (length board + 1) && board !! (row - 1) /= 0

selectRow :: Board -> IO Int
selectRow board = do
  putStr "Please select a row: "
  row <- getNatNum
  if validRow board row
    then return row
    else do
      putStrLn "Invalid row."
      selectRow board

validCount :: Board -> Int -> Int -> Bool
validCount board row count = count > 0 && count <= board !! (row - 1)

selectCount :: Board -> Int -> IO Int
selectCount board row = do
  putStr ("Please select how many to remove from row " ++ show row ++ ": ")
  count <- getNatNum
  if validCount board row count
    then return count
    else do
      putStrLn "Invalid count."
      selectCount board row

-- logic
isComplete :: Board -> Bool
isComplete = foldr (\r -> (&&) (r == 0)) True

updateBoard :: Board -> Int -> Int -> Board
updateBoard board row count = [if r == row then c - count else c | (r, c) <- zip [1 ..] board]

play :: Board -> Int -> IO ()
play board player = do
  putBoard board
  if isComplete board
    then putStrLn ("The winnder is player " ++ show player ++ "!")
    else do
      putStrLn ("It's player " ++ show player ++ "'s turn.")
      row <- selectRow board
      count <- selectCount board row
      play (updateBoard board row count) (if player == 1 then 2 else 1)

-- NIM
nim :: IO ()
nim = do
  play [5, 4, 3, 2, 1] 1
