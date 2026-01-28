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
data Direction = North | East | South | West
data Turn = TNone | TLeft | TRight | TAround


-- rotate: Determine a new antenna direction after rotating.
rotate :: Turn -> Direction -> Direction
rotate TNone d = d
rotate TLeft North = West
rotate TLeft East = North
-- orient: find a rotation to change an orientation from the first given direction to the second one.
orient :: Direction -> Direction -> Turn


rotateMany :: Direction -> [Turn] -> Direction

rotateManySteps :: Direction -> [Turn] -> [Direction]

orientMany :: [Direction] -> [Turn]


rotateFromFile :: Direction -> FilePath -> IO ()
orientFromFile :: FilePath -> IO ()

main:: IO()
