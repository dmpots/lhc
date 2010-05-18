-- RUN: %lhc -c %s 
-- RUN: %lhc compile %b.hcr
-- RUN: %b > %t1 ; diff %b.expected.stdout %t1

main :: IO ()
main = print "hi"
