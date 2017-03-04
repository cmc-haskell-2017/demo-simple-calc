import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/SimpleCalc.hs"]
