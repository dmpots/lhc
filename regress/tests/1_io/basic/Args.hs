import System.Environment

main :: IO ()
main = do
    as <- getArgs
    mapM_ putStrLn as
