-- RUN: %lhc -c %s 
-- RUN: %lhc compile %b.hcr
-- RUN: %b Foo Bar Baz > %t1 ; diff %b.expected.stdout %t1
import System.Environment

main :: IO ()
main = do
    as <- getArgs
    mapM_ putStrLn as
