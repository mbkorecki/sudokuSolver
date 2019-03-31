solveSudoku :: [String] ->[String]
solveSudoku xs = let ans = solutionSudoku 0 0 xs in
  if (length ans) == 9
    then ans
    else ["The sudoku is incorrect (no solutions or more than one solution)."]
  where
    solutionSudoku 9 8 xs = xs
    solutionSudoku 9 n xs = solutionSudoku 0 (n + 1) xs

    solutionSudoku m n xs = if ((xs !! n) !! m) == '0'
      then let possibles = conflictsAll m n xs "123456789" in
        if ((length possibles) > 0)
          then possibleWorlds m n xs possibles
          else []
      else solutionSudoku (m + 1) n xs

    possibleWorlds _ _ _ [] = []
    possibleWorlds m n xs possibles = let newXs = replaceAtIndex n (replaceAtIndex m (possibles !! ((length possibles) - 1)) (xs !! n)) xs in
      (possibleWorlds m n xs (init possibles)) ++ (solutionSudoku (m + 1) n newXs)


    conflictsAll y x xs ys = conflictBox x y 2 2 xs (conflictRow 8 x xs (conflictCol 8 y xs ys))
    -- x - is the row number, y is the column number

    replaceAtIndex m item xs = a ++ (item:b) where (a, (_:b)) = splitAt m xs

    conflictBox _ _ 0 0 _ ys = ys
    conflictBox n m 0 v xs ys = conflictBox n m 2 (v - 1) xs (filter (/= ((xs !! (n)) !! (m + v))) ys)
    conflictBox n m u v xs ys = let
      n' = (3 * (quot (n) 3))
      m' = (3 * (quot (m) 3))  in
      conflictBox n' m' (u - 1) v xs (filter (/= ((xs !! (n' + u)) !! (m' + v))) ys)

    conflictRow 0 m xs ys = filter (/= ((xs !! m) !! 0)) ys
    conflictRow n m xs ys = conflictRow (n - 1) m xs (filter (/= ((xs !! m) !! n)) ys)

    conflictCol 0 m xs ys = filter (/= ((xs !! 0) !! m)) ys
    conflictCol n m xs ys = conflictCol (n - 1) m xs (filter (/= ((xs !! n) !! m)) ys)

wrapper :: String ->[String]
wrapper input = solveSudoku (lines input)

main = print.wrapper =<< getContents
