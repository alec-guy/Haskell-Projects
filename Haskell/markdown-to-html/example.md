    main :: IO () 
    main = do 
      putStr "Enter num: "
      hFlush stdout 
      num <- readLn :: IO Int 
      putStrLn  $ show $ num * 2 
      putStrLn "Code above is doubled"
