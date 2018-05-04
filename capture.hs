import Data.List


--capture :: [String] -> Char -> Int -> String
--capture previous control depth
-- return move selected by heuristic
-- do all_moves for my moves, then call all_moves on each result again for opponent's move
-- call heuristic() to evaluate, do minimax

-- "-wWw--www-------bbb--bBb-"
-- 0,0 is in the top right corner
get_y :: Int -> Int
get_y z = 
	div z 5

get_x :: Int -> Int
get_x z = 
	mod z 5

index :: Int -> Int -> Int
index x y = 
	x + (y * 5)

unit_at :: String -> Int -> Int -> Char
unit_at state x y 
	| (out_of_bounds x y) = 'E' 
	| otherwise 		= state !! (index x y)

out_of_bounds :: Int -> Int -> Bool
out_of_bounds x y = 
	0 > x || x > 4 || 0 > y || y > 4


forward :: Char -> Int
forward control
	| control == 'w' = 1
	| control == 'b' = -1

is_enemy :: Char -> Char -> Bool
is_enemy me other
	| (me == 'w' || me == 'W') = (other == 'b' || other == 'B')
	| (me == 'b' || me == 'B') = (other == 'w' || other == 'W')
	| otherwise 			   = False

my_unit :: Char -> Char -> Bool
my_unit me unit
	| (me == 'w' || me == 'W') = (unit == 'w' || unit == 'W')
	| (me == 'b' || me == 'B') = (unit == 'b' || unit == 'B')
	| otherwise 			   = False


dx :: Char -> Int -> Int
dx unit i
	| (unit == 'w' || unit == 'b') = pawn_dx unit i 
	| (unit == 'W' || unit == 'B') = flag_dx unit i 

dy :: Char -> Int -> Int
dy unit i
	| (unit == 'w' || unit == 'b') = pawn_dy unit i 
	| (unit == 'W' || unit == 'B') = flag_dy unit i 

-- pawn moves:
-- (-1, 0)
-- (1, 0)
-- (0, forward)
-- (-2, 0)
-- (2, 0)
-- (0, 2 * forward)
pawn_dx :: Char -> Int -> Int
pawn_dx control i
	| i == 0 = (-1)
	| i == 1 = 1
	| i == 2 = 0
	| i == 3 = (-2)
	| i == 4 = 2
	| i == 5 = 0
pawn_dy :: Char -> Int -> Int
pawn_dy control i
	| i == 0 = 0
	| i == 1 = 0
	| i == 2 = (forward control)
	| i == 3 = 0
	| i == 4 = 0
	| i == 5 = 2 * (forward control)

-- flag moves:
-- (-1, 0)
-- (1, 0)
-- (0, 1)
-- (0, -1)
flag_dx :: Char -> Int -> Int
flag_dx control i
	| i == 0 = (-1)
	| i == 1 = 1
	| i == 2 = 0
	| i == 3 = 0
flag_dy :: Char -> Int -> Int
flag_dy control i
	| i == 0 = 0
	| i == 1 = 0
	| i == 2 = 1
	| i == 3 = (-1)


slice :: Int -> Int -> String -> String
slice from to xs = take (to - from + 1) (drop from xs)

print_5x5 :: String -> IO ()
print_5x5 state = do
	putStrLn (slice 0 4 state)
	putStrLn (slice 5 9 state)
	putStrLn (slice 10 14 state)
	putStrLn (slice 15 19 state)
	putStrLn (slice 20 24 state)

print_list :: [String] -> IO ()
print_list list
	| length list > 0 = do
		print_5x5 (head list)
		putStrLn " "
		print_list (tail list)
	| otherwise = return ()


-- go through board, get all pieces of same faction
-- get possible moves for them
-- substract previous states (\\)
all_moves :: Char -> [String] -> String -> [String]
all_moves control previous state =
	concat (map (\z -> moves control previous state z) [0..24])


-- get a list of possible moves(as strings of resulting states)
-- 6 moves for pawns, 4 moves for flags
moves :: Char -> [String] -> String -> Int -> [String]
moves control previous state z
	| (unit_at state (get_x z) (get_y z)) == 'w' || (unit_at state (get_x z) (get_y z)) == 'b' =
		(moves_helper state (get_x z) (get_y z) 5) \\ previous
	| (unit_at state (get_x z) (get_y z)) == 'W' || (unit_at state (get_x z) (get_y z)) == 'B' =
		(moves_helper state (get_x z) (get_y z) 3) \\ previous
	| otherwise 						  = []

moves_helper :: String -> Int -> Int -> Int -> [String]
moves_helper state x y i
	| i < 0  																			  = []
	| (move_is_legal state x y (dx (unit_at state x y) i) (dy (unit_at state x y) i)) 	  = 
		[(apply_move state x y (dx (unit_at state x y) i) (dy (unit_at state x y) i))] 
		++ (moves_helper state x y (i - 1))
	| otherwise 																		  = 
		moves_helper state x y (i - 1)


-- not out of bounds
-- destination is empty
-- if jumping(dx or dy > 2) there is an enemy to jump over
move_is_legal :: String -> Int -> Int -> Int -> Int -> Bool 
move_is_legal state x y dx dy =
	not(out_of_bounds (x + dx) (y + dy))
	&& (unit_at state (x + dx) (y + dy)) == '-'
	&& (((abs dx) < 2 && (abs dy) < 2) 
		|| (is_enemy (unit_at state x y) (unit_at state (x + (signum dx)) (y + (signum dy)))))


set_at :: String -> Int -> Int -> Char -> String
set_at list x y unit =
	take (index x y) list ++ [unit] ++ drop ((index x y) + 1) list


-- erase unit in front if jumping
-- draw unit at new position
-- erase unit at old position
apply_move :: String -> Int -> Int -> Int -> Int -> String
apply_move state x y dx dy
	| ((abs dx) == 2) || ((abs dy) == 2) =
		set_at (set_at (set_at state
			(x + (signum dx)) (y + (signum dy)) '-')
			(x + dx) (y + dy) (unit_at state x y))
			x y '-'
	| otherwise =
		(set_at (set_at state 
			(x + dx) (y + dy) (unit_at state x y))
			x y '-') 