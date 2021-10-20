module Module55 where

main' :: IO ()
main' = do
  putStrLn "What is your name?"
  putStr "Name: "
  input <- getLine
  if input == ""
    then main'
    else putStrLn $ "Hi, " ++ input ++ "!"
