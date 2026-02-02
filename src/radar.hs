{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

import Fmt
import Fmt (Buildable(..), Builder)
import System.Environment (getArgs)
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
            -> If a data type supports the idea of minimum and maximum values, then we define the Bounded instance.
-}
data Direction = North | East | South | West deriving (Eq, Enum, Bounded, Show)
data Turn = TNone | TLeft | TRight | TAround deriving (Eq, Enum, Bounded, Show)

{-
    If values of some type can be compared for equality, enumerated, and have bounds, then we can enumerate them in a cycle.
-}
class (Eq a, Enum a, Bounded a) => CyclicEnum a where
    cpred :: a -> a
    cpred d
        | d == minBound = maxBound
        | otherwise = pred d

    csucc :: a -> a
    csucc d
        | d == maxBound = minBound
        | otherwise = succ d

instance CyclicEnum Direction


{-
    Combining turns with Semigroup and Monoid

    Note that rotating a radar antenna twice to the left is the same as rotating it twice to the right. Turning an antenna around and then to the left
    is the same as turning it right in the first place. Combining turns is an example of a generic binary operation.

    The Semitgroup type class declares a combination operator as follows:
            (<>) :: Semigroup a => a -> a -> a

    The Monoid type class extends Semigroup and adds another method
        mempty :: Monoid a => a
    which defines a so-called neutral element.

    This element is called neutral because it is expected to satisfy the monoid laws, so that for any element a,

        mempty <> a == a
        a <> mempty == a

    In general, we use the (<>) operation to accumulate information, such as concatenating lists and other containers (Data.Set or Data.Sequence),
    combining configuration properties, and so forth. When encountering a new data type, it is always a good idea to look for provided
    instances. If we see instances for SemiGroup or Monoid, then we know that values of this type can be combined with (<>).

    Besides (<>) and mempty, other methods are available in Semigroup and Monoid. For example, we can apply a binary operation to a list
    of values, as follows:
        mconcat :: Monoid a => [a] -> a

    The mconcat function returns mempty if the given list is empty and applies an operation over all the elements from left to right otherwise.

    The Semigroup type class defines the sconcat function, which is similar to mconcat with one exception: there is no neutral element in the
    Semigroup, so we wouldn't be able to return something meaningful in the case of an empty list.
        sconcat :: Semigoup a => NonEmpty a -> a

    Now let's check all the combinations of Turn values, in a table, which is called a Cayley table:

    ----------------------------------------------------------
    <>                  |   TNone |  TLeft  |  TRight |  TAround
    ----------------------------------------------------------
    TNone          |   TNone |  TLeft  |  TRight  |  TAround
    ----------------------------------------------------------
    TLeft           |  TLeft  |  TAround  |  TNone  | TRight
    ----------------------------------------------------------
    TRight       |  TRight  |  TNone  | TAround  | TLeft
    ----------------------------------------------------------
    TAround |  TAround  |  TRight  |  TLeft  |  TNone
    ----------------------------------------------------------

    Note that our Semigroup of rotations is commutative, meaning that t1 <> t2 == t2 <> t1. This simplifies the function definition.
-}
instance Semigroup Turn where
    TNone <> t = t
    TLeft <> TLeft = TAround
    TLeft <> TRight = TNone
    TLeft <> TAround = TRight
    TRight <> TRight = TAround
    TRight <> TAround = TLeft
    TAround <> TAround = TNone
    t1 <> t2 = t2 <> t1 -- employs commutativity, to define the lower half of the table.

instance Monoid Turn where
    mempty = TNone

-- rotate: Determine a new antenna direction after rotating.
rotate :: Turn -> Direction -> Direction
rotate TNone = id
rotate TLeft = cpred
rotate TRight  = csucc
rotate TAround = cpred . cpred

{-
    Because we have an instance of Enum for the Turn datatype, this list expression can be shortened to [TNone .. TAround].
    Also, because we have Bounded, it can be [minBound .. maxBound] - and this can be abstracted again as a list with every
    value of a bounded enumerable type.
-}
every :: (Enum a, Bounded a) => [a]
every = enumFrom minBound

{-
    Demystifying the "OverloadedStrings" GHC extension

    When we enable it, types for String literals become more generic.
    The IsString type class defines only one method, fromString :
        fromString :: IsString a => String -> a

    The only thing the extension OverloadedStrings is responsible for is replacing every string literal in the source code
    with a call to the fromString method on that literal. Then it's time for instance resolution algorithms to find the right
    instance and convert a String to some other type. Note, it should be unambiguous from the context which type is
    expected at the position of a String literal.
-}
-- orient: find a rotation to change an orientation from the first given direction to the second one.
orient :: Direction -> Direction -> Turn
orient d1 d2 = head $ filter (\t -> rotate t d1 == d2)
                                        [TNone, TLeft, TRight, TAround]

orient' :: Direction -> Direction -> Turn
orient' d1 d2 = head $ filter (\t -> rotate t d1 == d2) every

rotateMany :: Direction -> [Turn] -> Direction
rotateMany = foldl' (flip rotate)

{-
Instead of rotating the antenna many times to get to the final direction, we could compute the combined turns and
rotate it only once:
-}
rotateMany' :: Direction -> [Turn] -> Direction
rotateMany' dir ts = rotate (mconcat ts) dir

rotateManySteps :: Direction -> [Turn] -> [Direction]
rotateManySteps = scanl (flip rotate)

orientMany :: [Direction] -> [Turn]
orientMany ds@(_:_:_) = zipWith orient ds (tail ds)
orientMany _ = []

deriving instance Read Direction
deriving instance Read Turn

instance Buildable Direction where
    build North = "N"
    build East = "E"
    build South = "S"
    build West = "W"

instance Buildable Turn where
    build TNone = "--"
    build TLeft = "<-"
    build TRight = "->"
    build TAround = "||"

{-
    Ordering is built upon equality and adds several operators and functions. It is enough to implement only one function, compare, or one
    operator (<=), to get all the other functions for free.
-}
deriving instance Ord Turn

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile dir fname = do
    f <- readFile fname
    let
        turns = map read $ lines f
        finalDir = rotateMany dir turns
        dirs = rotateManySteps dir turns
    fmtLn $ "Final Direction: "+||finalDir||+"" -- Using show instance (note +|| and ||+ operators)
    fmt $ nameF "Intermediate Directions" (unwordsF dirs)   -- Using buildable instance

orientFromFile :: FilePath -> IO ()
orientFromFile fname = do
    f <- readFile fname
    let dirs = map read $ lines f
    fmt $ nameF "All turns" (unwordsF $ orientMany dirs)

main:: IO()
main = do
    args  <- getArgs
    case args of
        ["-r", fname, dir]  -> rotateFromFile (read dir) fname
        ["-o", fname] -> orientFromFile fname
        _ -> putStrLn $ "Usage: radar -o filename\n " ++
                                 "            radar -r filename direction"
