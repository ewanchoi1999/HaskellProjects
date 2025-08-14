Connect 4 Game in Haskell

----------------------------------------------------------------------

We define constants for the row and column size of the board, length
of a winning sequence, and search depth for the game tree:

> import Data.List

> rows :: Int
> rows = 6
>
> cols :: Int
> cols = 7
>
> win :: Int
> win = 4
>
> depth :: Int
> depth = 6

The board itself is represented as a list of rows, where each row is
a list of player values, subject to the above row and column sizes:

> type Board = [Row]
>
> type Row = [Player]

In turn, a player value is either a nought, a blank, or a cross, with
a blank representing a position on the board that is not yet occupied:

> data Player = O | B | X
>               deriving (Ord, Eq, Show)

For example, here is a typical board:

> test :: Board
> test = [[B,B,B,B,B,B,B],
>         [B,B,B,B,B,B,B],
>         [B,B,B,B,B,B,B],
>         [B,B,B,X,X,B,B],
>         [B,B,O,O,X,B,B],
>         [B,O,O,X,X,X,O]]

The following code displays a board on the screen:

> showBoard :: Board -> IO ()
> showBoard b = putStrLn (unlines (map showRow b ++ [line] ++ [nums]))
>               where
>                  showRow = map showPlayer
>                  line    = replicate cols '-'
>                  nums    = take cols ['0'..]
>
> showPlayer :: Player -> Char
> showPlayer O = 'O'
> showPlayer B = '.'
> showPlayer X = 'X'



Create an empty Board to start the game with 

> initBoard :: Board
> initBoard = [[B,B,B,B,B,B,B],
>         [B,B,B,B,B,B,B],
>         [B,B,B,B,B,B,B],
>         [B,B,B,B,B,B,B],
>         [B,B,B,B,B,B,B],
>         [B,B,B,B,B,B,B]]


Flatten the board into a single list and then calculate the number 
of x and o in the list. 
PlayerO turn if there are more x on the board or the game has begun. 
otherwise it is PlayerX's turn


> turn :: Board -> Player
> turn board = if numX <= numO then O else X
>     where
>         boardList = concat board
>         numX = length (filter (== X) boardList)
>         numO = length (filter (== O) boardList)

Get the row from a board as a list 

> getRowValues :: Board -> Int -> Row
> getRowValues b c = b !! c  

Get a column from the board as a list 

> getColValues :: Board -> Int -> [Player]
> getColValues b c = [row !! c | row <- b]

 fourRow :: Board -> Player 
 fourRow b = map (w❌y:z:xs row) b

> hasRow :: Player -> Row -> Bool 
> hasRow p r = or [all (== p) (take win (drop i r)) | i <- [0..(win-1)]]

> hasCol :: Player -> [Player] -> Bool 
> hasCol p r = or [all (== p) (take win (drop i r)) | i <- [0..(win-1)]]

> hasWon :: Player -> Board -> Bool
> hasWon p b = if hasRow p (head b) || hasCol p (head b) == True then True else False

----------------------------------------------------------------------
