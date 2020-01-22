import Debug.Trace

main = interact $ show . sum . map read . traceShowId . words
