{-
    Problem at Hand

    A radar antenna is a device with the ability to be oriented toward four points of direction (namely, north, south, east or west)
    We can rotate a radar antenna. It supports a limited set of commands to be turned left, right and all around. It can also stay
    in its current direction.

    The task is to write a program that manipulates a radar antenna. The following are the two modes of operation:

        -> When given a file with a list of turns and a starting direction, the program executes all the turns and reports the final
        direction with all the intermediate directions.
        -> When given a file with a list of directions, the program computes and reports the corresponding set of turns to orient
        the radar antenna as required.
-}
data Direction = North | East | South | West deriving (Eq, Enum, Bounded, Show)
data Turn = TNone | TLeft | TRight | TAround deriving (Eq, Enum, Bounded, Show)


-- rotate: Determine a new antenna direction after rotating.
rotate :: Turn -> Direction -> Direction
rotate TNone d = d
rotate TLeft North = West
rotate TLeft East = North
-- orient: find a rotation to change an orientation from the first given direction to the second one.
orient :: Direction -> Direction -> Turn
orient _ _ = TNone

rotateMany :: Direction -> [Turn] -> Direction
rotateMany _ _ = North

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps _ _ = [North]

orientMany :: [Direction] -> [Turn]
orientMany _ = [TNone]


rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile _ _ = do
    putStrLn "TODO"

orientFromFile :: FilePath -> IO ()
orientFromFile _ = do
    putStrLn "TODO"

main:: IO()
main = do
    putStrLn "TODO"


{-
    In Haskell, we have the Eq type class with two methods, (==) and (/=).
        class Eq a where
            (==), (/=)  ::a -> a -> Bool

            {-# INLINE (/=) #-}
            {-# INLINE (/=) #-}
            x /= y  = not (x == y)      -- Default method implementation
            x == y  = not (x /= y)

            {-# MINIMAL (==) | (/=) #-} -- Minimal complete definition: It's enough to implement either (==) or (/=)


    Other useful concepts are boundedness and enumeration. In Haskell they are represented by the type classes
    Bounded (for types with minimum and maximum bounds) and Enum (for types whose values can be enumerated by the
    Int values).

        class Bounded a where
            minBound: a
            maxBound: a

            {-# MINIMAL minBound, maxBound #-}

        class Enum a where
            succ :: a -> a
            pred :: a -> a
            toEnum :: Int -> a
            fromEnum :: a -> Int
            enumFrom :: a -> [a]
            enumFromThen :: a -> a -> [a]
            enumFromTo :: a -> a -> [a]
            enumFromThenTo :: a -> a -> a -> [a]
            {-# MINIMAL toEnum, fromEnum #-}

    Intuition:
        -> If the values of a type can be checked for equality, we define an instance of the Eq type class
        -> If it makes sense to enumerate elements of a type one by one, then it is a good sign that we should
        implement Enum. The moment we do that, we get a bunch of methods we can use with its values.
        -> If a data type suppports the idea of minimum and maximum values, then we define the Bounded instance.

-}
