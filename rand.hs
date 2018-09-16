import System.Random
import Data.Time.Clock.POSIX
import Data.Char

(|>) :: a -> (a -> b) -> b
(|>) x f = f x
infixl 0 |>

removeFirst :: (a -> Bool) -> [a] -> Maybe [a]
removeFirst p []     = Nothing
removeFirst p (x:xs) = if p x
                       then Just xs
                       else case removeFirst p xs of
                            Just xs' -> Just $ x : xs'
                            Nothing -> Nothing
replaceFirst :: (a -> Bool) -> a -> [a] -> Maybe [a]
replaceFirst p y []     = Nothing
replaceFirst p y (x:xs) = if p x
                          then Just $ y : xs
                          else case replaceFirst p y xs of
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


data Prize = Goat | Car
    deriving (Show, Eq)

data Info = Unknown | RevealedBad | Chosen 
    deriving (Show, Eq)

type Door = (Prize, Info)

doorN = 3

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
    let
        filt = filter (\(p, inf) -> inf == Unknown) ds
        imod = mod rnd $ length filt
        (re_p, re_inf) = filt !! imod
    in
        [(re_p, Chosen)]

lap :: Bool -> Int -> Int -> Bool
lap switch rnd1 rnd2 =
    ds3 |> filter isChosen |> head |> (==) (Car, Chosen)
    where
        ds0 = constructDoors
        ds1 = chooseDoor rnd1 ds0
        ds2 = unvealGoat ds1
        ds3 = if switch
              then switchDoors rnd2 ds2
              else ds2

type Result = (Int, Int)

loop :: Bool -> [Int] -> Int -> Result
loop switch = loopt (0, 0)
    where
        loopt :: Result -> [Int] -> Int -> Result
        loopt prev         _            0     = prev
        loopt (win, los) (r0 : r1 : rs) times = loopt curr rs (times - 1)
            where
                curr = if lap switch r0 r1 then (win + 1, los) else (win, los + 1)

main = do
    t <- getPOSIXTime
    rs <- return $ randoms $ mkStdGen $ round t
    
    putStr "Switch doors? [Y/n] "
    switchStr <- getLine
    switch <- return $ case map toLower switchStr of 
                       "n" -> False
                       _   -> True

    (win, los) <- return $ loop switch rs 10000
    perc <- return $ round $ (fromIntegral win) / (fromIntegral $ win + los) * 100
    
    putStrLn $ "Result: " ++ show win ++ "/" ++ (show $ los + win)
        ++ " (" ++ show perc ++ "%)"

