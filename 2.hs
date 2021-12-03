type Command = (String, Int)

data Pos = Pos
  { depth :: Int,
    hPos  :: Int,
    aim :: Int
    } deriving (Show)

parseLine :: String -> Command
parseLine l = do
    let x = words l
    (head x, read $ head $ tail x)

positionFromCommands :: Pos -> [Command] -> Pos
positionFromCommands p [] = p
positionFromCommands p (c:cmds) = case c of 
                                    ("forward", x) -> positionFromCommands p{hPos = hPos p + x} cmds
                                    ("up", x)      -> positionFromCommands p{depth = depth p - x} cmds
                                    ("down", x)    -> positionFromCommands p{depth = depth p + x} cmds

posWithAim :: Pos -> [Command] -> Pos
posWithAim p []= p
posWithAim p (c:cmds) = case c of 
                        ("forward", x) -> posWithAim p{hPos = hPos p + x, depth = depth p + (aim p * x)} cmds
                        ("up", x)      -> posWithAim p{aim = aim p - x} cmds
                        ("down", x)    -> posWithAim p{aim = aim p + x} cmds
main = do
    content <- readFile "2.txt"
    let commands = map parseLine $ lines content

    let newPosition = positionFromCommands Pos{depth=0,hPos=0,aim=0} commands
    print $ (hPos newPosition) * (depth newPosition)

    let newPos= posWithAim Pos{depth=0,hPos=0,aim=0} commands
    print $ (hPos newPos) * (depth newPos)

