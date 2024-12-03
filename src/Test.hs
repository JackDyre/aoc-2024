module Test where

runTest :: (Eq a, Eq b, Show a, Show b) => String -> (String -> (a, b)) -> FilePath -> (a, b) -> IO ()
runTest name f path (a', b') = do
  (a, b) <- f <$> readFile path
  putStrLn ""
  putStrLn $ validate a a' "Pt 1"
  putStrLn $ validate b b' "Pt 2"
  putStrLn ""
  where
    validate got expected label =
      if got == expected
        then unwords ["(Passed)", name, label ++ ":", "[" ++ show got ++ "]"]
        else unwords ["(Failed)", name, label ++ ":\n      Got     ", "[" ++ show got ++ "]\n      Expected", "[" ++ show expected ++ "]"]
