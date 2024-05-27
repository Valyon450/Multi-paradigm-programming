import Data.List (sort, group, elemIndex)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Char (isSpace)
import System.CPUTime
import Text.Printf (printf)

-- Функція для розбивання чисельного ряду на інтервали, використовуючи рівномірний розподіл
splitIntoIntervals :: [Double] -> Int -> [[Double]]
splitIntoIntervals xs n = 
  let sorted = sort xs
      (minVal, maxVal) = (head sorted, last sorted)
      intervalSize = (maxVal - minVal) / fromIntegral n
  in [ [minVal + fromIntegral i * intervalSize, minVal + fromIntegral (i+1) * intervalSize] | i <- [0..(n-1)] ]

-- Функція для визначення якому інтервалу належить число
assignInterval :: Double -> [[Double]] -> Int
assignInterval x intervals = fromMaybe (-1) $ elemIndex True $ map (\[a, b] -> x >= a && x <= b) intervals

-- Функція для перетворення числового ряду в лінгвістичний ряд
toLinguistic :: [Double] -> [Char] -> String
toLinguistic xs alphabet =
  let n = length alphabet
      intervals = splitIntoIntervals xs n
      assignInterval' x = assignInterval x intervals
      safeIndex i xs = if i < 0 || i >= length xs then '?' else xs !! i
  in map (\x -> safeIndex (assignInterval' x) alphabet) xs

-- Функція для створення матриці передування
createMatrix :: String -> String -> [[Int]]
createMatrix linguistic alphabet =
  let n = length alphabet
      counts = map (\x -> map (\y -> length (filter (\(a,b) -> a == x && b == y) (zip linguistic (tail linguistic)))) alphabet) alphabet
  in counts

-- Функція для читання чисельного ряду з файлу
readSeries :: FilePath -> IO [Double]
readSeries path = do
  content <- readFile path
  return $ map read (words content)

-- Функція для читання параметрів з файлу
readParams :: FilePath -> IO (Int, [Char])
readParams path = do
  content <- readFile path
  let (n:alphabet) = lines content
  return (read n, filter (not . isSpace) (concat alphabet))

-- Функція для запису результатів у консоль та у файл
writeResults :: FilePath -> String -> String -> IO ()
writeResults path alphabet linguistic = do
  let writeLinguistic = "Linguistic series: \n" ++ linguistic ++ "\n"
      alphabetHeader = intercalate "\t" (map (\x -> [x]) alphabet)
      matrixRows = map (\(c, row) -> c : "\t" ++ intercalate "\t" (map show row)) (zip alphabet (createMatrix linguistic alphabet))
      writeMatrix = "\nPrecedence matrix:\n\t" ++ alphabetHeader ++ "\n" ++ unlines matrixRows
  printf "%s" writeLinguistic
  printf "%s" writeMatrix
  writeFile path (writeLinguistic ++ writeMatrix)

-- Функція процесу повної обробки ряду
processSeries :: FilePath -> FilePath -> FilePath -> IO ()
processSeries seriesPath paramsPath outputPath = do
  series <- readSeries seriesPath
  (alphabetSize, alphabet) <- readParams paramsPath
  let linguistic = toLinguistic series alphabet
  writeResults outputPath alphabet linguistic

-- Головна функція
main :: IO ()
main = do
  start <- getCPUTime
  processSeries "number series.txt" "params.txt" "output.txt"
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12)
  printf "Data written to output.txt\nExecution time: %0.3f sec\n" (diff :: Double)
