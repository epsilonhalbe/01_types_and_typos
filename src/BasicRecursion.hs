import System.IO (hSetBuffering, BufferMode(..), stdin)

main :: IO ()
main = do hSetBuffering stdin LineBuffering
          basicRecursion

basicRecursion :: IO ()
basicRecursion = do putStrLn "Guess my word:"
                    word <- getInt
                    if word == 1 then putStrLn "Word up!"
                                 else basicRecursion


getInt :: IO Double
getInt = do str <- getLine
            return (read str)
