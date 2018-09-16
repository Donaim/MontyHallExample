import System.Random
import Data.Time.Clock.POSIX

data Prize = Goat | Car
    deriving (Show, Eq)

(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>

doorN = 3
switch = True

data Info = Unknown | RevealedBad | Chosen 
    deriving (Show, Eq)

type Door = (Prize, Info)

removeFirst :: (a -> Bool) -> [a] -> Maybe [a]
removeFirst p []     = Nothing
removeFirst p (x:xs) = if p x
                       then Just xs
                       else case removeFirst p xs of
                            Just xs' -> Just $ x : xs'
                            Nothing -> Nothing

removeAt :: Int -> [a] -> Maybe [a]
removeAt _ [] = Nothing
removeAt 0 (_:xs) = Just xs
removeAt i (x:xs) = case removeAt (i - 1) xs of
                    Just xs' -> Just $ x : xs'
                    Nothing  -> Nothing

unwrape :: String -> Maybe a -> a
unwrape _   (Just x) = x
unwrape err Nothing  = error err
unwrap = unwrape "Not expected to fail"

constructDoors :: [Door]
constructDoors =
    Car : [Goat | _ <- [2..doorN]] |> map (\x -> (x, Unknown))

chooseDoor :: Int -> [Door] -> [Door]
chooseDoor rnd ds =
    ds |> removeAt imod |> unwrap |> (:) (prize, Chosen)
    where
        imod = mod rnd $ length ds
        (prize, _) = ds !! imod

unvealGoat :: [Door] -> [Door]
unvealGoat ds =
    ds |> removeFirst isUnknownGoat |> unwrap |> (:) (Goat, RevealedBad)
    where
        isUnknownGoat :: Door -> Bool
        isUnknownGoat (Goat, Unknown) = True
        isUnknownGoat _               = False

isChosen :: Door -> Bool
isChosen (_, Chosen) = True
isChosen _           = False

switchDoors :: Int -> [Door] -> [Door]
switchDoors rnd ds =
    ds |> removeFirst isToSwap |> unwrap 
       |> removeFirst isChosen |> unwrap 
       |> (:) (swapPrize, Chosen)
       |> (:) (chosenPrize, Unknown)
    where
        isToSwap :: Door -> Bool
        isToSwap (_, Unknown) = True
        isToSwap _            = False

        (chosenPrize, _) = ds |> filter isChosen |> head
        (swapPrize, _)   = ds |> filter isToSwap |> head

lap :: Int -> Int -> Bool
lap rnd1 rnd2 =
    ds3 |> filter isChosen |> head |> (==) (Car, Chosen)
    where
        ds0 = constructDoors
        ds1 = chooseDoor rnd1 ds0
        ds2 = unvealGoat ds1
        ds3 = if switch
              then switchDoors rnd2 ds2
              else ds2

type Result = (Int, Int)

loop :: [Int] -> Int -> Result
loop = loopt (0, 0)
    where
        loopt :: Result -> [Int] -> Int -> Result
        loopt prev       _              0     = prev
        loopt (win, los) (r0 : r1 : rs) times = loopt curr rs (times - 1)
            where
                curr = if lap r0 r1 then (win + 1, los) else (win, los + 1)

main = do
    t <- getPOSIXTime
    rs <- return $ randoms $ mkStdGen $ round t

    (win, los) <- return $ loop rs 10000
    perc <- return $ round $ (fromIntegral win) / (fromIntegral $ win + los) * 100
    
    print $ "Switch: " ++ show switch
    print $ "Result: " ++ show win ++ "/" ++ (show $ los + win)
        ++ " (" ++ show perc ++ "%)"

