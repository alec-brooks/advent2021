type Command = (String, Int)

type Position = (Int, Int)

data Pos = Pos
  { depth :: Int,
    hPos  :: Int,
    aim :: Int
    } deriving (Show)

parseLine :: String -> Command
parseLine l = do
    let x = words l
    (head x, read $ head $ tail x)

positionFromCommands :: Position -> [Command] -> Position
positionFromCommands p [] = p
positionFromCommands p (c:cmds) = case c of 
                                    ("forward", x) -> positionFromCommands (fst p + x, snd p) cmds
                                    ("up", x)      -> positionFromCommands (fst p, snd p - x) cmds
                                    ("down", x)    -> positionFromCommands (fst p, snd p + x) cmds

posWithAim :: Pos -> [Command] -> Pos
posWithAim p []= p
posWithAim p (c:cmds) = case c of 
                        ("forward", x) -> posWithAim p{hPos = hPos p + x, depth = depth p + (aim p * x)} cmds
                        ("up", x)      -> posWithAim p{aim = aim p - x} cmds
                        ("down", x)    -> posWithAim p{aim = aim p + x} cmds
main = do
    content <- readFile "2.txt"

    let linesOfFile = lines content
    let commands = map parseLine linesOfFile

    let newPosition = positionFromCommands (0,0) commands
    print $ (fst newPosition) * (snd newPosition)

    let newPos= posWithAim Pos{depth=0,hPos=0,aim=0} commands
    print $ (hPos newPos) * (depth newPos)

