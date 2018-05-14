import Data.List


-- TODO
-- move checking for repeated states into move_is_legal(remove from everywhere else)
-- implement games between one heuristic vs list of heuristics, might be needed later for when we'll have to test against many heuristics
-- clean up driver, maybe send to teacher
-- add typing and comments to all functions
-- rearrange functions??? tried to keep it C style, but I think it would be better to group by type(helpers on bottom, main routines on top)
-- remove hardcoded width of 5 from everything, replace with width = sqrt (length state)

start_game = game_hvh ["-wWw--www-------bbb--bBb-"] 'w'


h_static :: [String] -> Char -> Int
h_static previous control = 0

h_pawn_count :: [String] -> Char -> Int
h_pawn_count previous control 
	| is_win previous control 	= 1000
	| is_win previous enemy 	= -1000 
	| otherwise 				= (count_chars state control)
	where
		state = (head previous)
		enemy = opposite control

h_pawn_count2 :: [String] -> Char -> Int
h_pawn_count2 previous control 
	| is_win previous control 	= 1000
	| is_win previous enemy 	= -1000 
	| otherwise 				= (count_chars state control) - (count_chars state enemy)
	where
		state = (head previous)
		enemy = opposite control

h_flag_y :: [String] -> Char -> Int
h_flag_y previous control 
	| is_win previous control 	= 1000
	| is_win previous enemy 	= -1000 
	| control == 'w' 			= maybe 0 get_y (elemIndex 'W' state)
	| otherwise					= maybe 0 get_y (elemIndex 'B' (reverse state))
	where
		state = (head previous)
		enemy = opposite control

h_flag_to_pawns_distance :: [String] -> Char -> Int
h_flag_to_pawns_distance previous control 
	| is_win previous control 	= 1000
	| is_win previous enemy 	= -1000 
	| control == 'w' 			= flag_pawns_distance_w (head previous)
	| otherwise					= flag_pawns_distance_b (head previous)
	where
		state = (head previous)
		enemy = opposite control
flag_pawns_distance_w state =
	0 - (count_chars (drop ((maybe 0 get_y (elemIndex 'W' state)) * 5) state) 'b')
flag_pawns_distance_b state =
	0 - (count_chars (drop ((maybe 0 get_y (elemIndex 'B' state)) * 5) reversed) 'w')
	where
		reversed = reverse state



-- heuristic vs heuristic
game_hvh previous control
	| is_win previous control = do
		print control
		print_5x5 (head previous)
		print control
		print "wins!"
		print (why_won previous control)
	| is_win previous enemy = do
		print enemy
		print_5x5 (head previous)
		print enemy
		print "wins!"
		print (why_won previous enemy)
	| control == 'w' 		  = do
		putStrLn "Minimax's turn, Current board:"
		print_5x5 (head previous)
		game_hvh ((fst (minimax previous control control 2 w_heuristic)):previous) enemy  
	| otherwise   		  	  = do
		putStrLn "Minimax's turn, Current board:"
		print_5x5 (head previous)
		game_hvh ((fst (minimax previous control control 2 b_heuristic)):previous) enemy  
	where
		enemy       = opposite control
		w_heuristic = h_static
		b_heuristic = h_pawn_count2








opposite control
	| control == 'w' = 'b'
	| otherwise 	 = 'w'

num_pawns state control
	| control == 'w' = (count_chars state 'w') 
	| otherwise = (count_chars state 'b') 

num_pieces state control
	| control == 'w' = (count_chars state 'w') + (count_chars state 'W') 
	| otherwise = (count_chars state 'b') + (count_chars state 'B') 

flag control
	| control == 'w' = 'W'
	| otherwise 	 = 'B'

flag_reached_end state control
	| control == 'w' = elem 'W' (drop 20 state)
	| otherwise		 = elem 'B' (take 5 state)


flag_passed_pawns state control
	| control == 'w' = flag_passed_pawns_w state
	| otherwise		 = flag_passed_pawns_b state
flag_passed_pawns_w state =
	not (elem 'b' (drop ((maybe 0 get_y (elemIndex 'W' state)) * 5) state))
flag_passed_pawns_b state =
	not (elem 'w' (drop ((maybe 0 get_y (elemIndex 'B' reversed)) * 5) reversed))
	where
		reversed = reverse state
	

--win if opponent has no moves
--if opponent has no pawns
--if opponent has no flag
--if your flag reached the opposite end of the board(y==4 for white, y==0 for black)

is_win previous control = 
	(num_pawns state enemy) == 0 --captured all pawns
	|| (count_chars state (flag enemy)) == 0 --captured flag
	|| null (all_moves previous enemy) --enemy can't make a legal move
	|| (flag_passed_pawns state control)
	where 
		state = (head previous)
		enemy = opposite control

why_won previous control 
	| (num_pawns state enemy) == 0  		= "captured all pawns"
	| (count_chars state (flag enemy)) == 0 = "captured flag"
	| null (all_moves previous enemy)       = "enemy can't make a legal move"
	| (flag_passed_pawns state control)     = "flag moved past all pawns"
	| otherwise								= "didn't actually win"
	where 
		state = (head previous)
		enemy = opposite control





--player vs player
game_pvp previous control
	| is_win previous control = do
		print control
		print " wins!"
	| otherwise = do
		putStrLn "Current:"
		print control
		print_5x5 (head previous)
		putStr "Possible moves:\n"
		print_list (all_moves previous control)
		move_index <- readLn
		game_pvp (((all_moves previous control) !! move_index):previous) (opposite control) 


readInts :: IO [Int]
readInts = fmap (map read.words) getLine

read4Ints :: [String] -> Char -> IO [Int]
read4Ints previous control = do
     ints <- readInts
     if (length ints == 4 
     	&& (move_is_legal (head previous) (ints!!0) (ints!!1) (ints!!2) (ints!!3))
     	&& not (elem (apply_move (head previous) (ints!!0) (ints!!1) (ints!!2) (ints!!3)) previous)
     	&& ((control == 'w' && (ints!!3) > 0) || (control == 'b' && (ints!!3) < 0))) 
		then return ints else do
         putStrLn ("Incorrect move, either wrong format or move is illegal")
         read4Ints previous control

-- player vs heuristic
game_pvh previous control
	| is_win previous control = do
		print control
		print " wins!"
	| control == 'b' 		  = do
		putStrLn "Minimax's turn, Current board:"
		print_5x5 (head previous)
		game_pvh ((fst (minimax previous control control 2 heuristic)):previous) (opposite control)  
	| otherwise 			  = do
		putStrLn "Player's turn, Current board:"
		print_5x5 (head previous)
		move_input <- read4Ints previous control
		game_pvh ((apply_move (head previous) 
			(move_input!!0) (move_input!!1) (move_input!!2) (move_input!!3)):previous) (opposite control)
	where
		heuristic = h_static

compare_states :: (String, Int) -> (String, Int) -> Ordering
compare_states x y
	| (snd x) < (snd y)  = LT
	| (snd x) == (snd y) = EQ
	| otherwise			 = GT




minimax :: [String] -> Char -> Char -> Int -> ([String] -> Char -> Int) -> (String, Int)
minimax previous control max_control depth heuristic
	| (depth == 0) || (is_win previous control) || (is_win previous (opposite control)) = 
		(head previous, heuristic previous max_control) --reached max depth
	| control == max_control 				= 
		(fst max, (snd max))
	| otherwise			 				= 
		(fst min, (snd min))
	where
		max = (maximumBy compare_states (map 
			(\new_state -> minimax (new_state:previous) (opposite control) max_control (depth - 1) heuristic) 
			(all_moves previous control)))
		min = (minimumBy compare_states (map 
			(\new_state -> minimax (new_state:previous) (opposite control) max_control (depth - 1) heuristic) 
			(all_moves previous control)))


count_chars :: String -> Char -> Int
count_chars str c = length $ filter (== c) str



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


forward :: Char -> Int
forward control
	| control == 'w' = 1
	| control == 'b' = -1

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


get_dx :: Char -> Int -> Int
get_dx unit i
	| (unit == 'w' || unit == 'b') = pawn_dx unit i 
	| (unit == 'W' || unit == 'B') = flag_dx unit i 

get_dy :: Char -> Int -> Int
get_dy unit i
	| (unit == 'w' || unit == 'b') = pawn_dy unit i 
	| (unit == 'W' || unit == 'B') = flag_dy unit i 
	

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
print_list list = print_list2 list 0

print_list2 :: [String] -> Int -> IO ()
print_list2 list i
	| length list > 0 = do
		print i
		print_5x5 (head list)
		print_list2 (tail list) (i + 1)
	| otherwise = return ()




-- go through board, get all pieces of same faction
-- get possible moves for them
-- substract previous states (\\)
all_moves :: [String] -> Char -> [String]
all_moves previous control =
	concat (map (\z -> moves previous control (head previous) z) [0..24])


-- get a list of possible moves(as strings of resulting states)
-- 6 moves for pawns, 4 moves for flags
moves :: [String] -> Char -> String -> Int -> [String]
moves previous control state z
	| (my_unit control unit) && (unit == 'w' || unit == 'b') 	=
		(moves_helper state (get_x z) (get_y z) 5) \\ previous
	| (my_unit control unit)									=
		(moves_helper state (get_x z) (get_y z) 3) \\ previous
	| otherwise 												= []
	where 
		unit = unit_at state (get_x z) (get_y z)

moves_helper :: String -> Int -> Int -> Int -> [String]
moves_helper state x y i
	| i < 0  								= []
	| (move_is_legal state x y dx dy) 	  	= [(apply_move state x y dx dy)] ++ (moves_helper state x y (i - 1))
	| otherwise 							= moves_helper state x y (i - 1)
	where
		dx = get_dx (unit_at state x y) i
		dy = get_dy (unit_at state x y) i


-- not out of bounds
-- destination is empty
-- if jumping(dx or dy > 2) there is an enemy to jump over
move_is_legal :: String -> Int -> Int -> Int -> Int -> Bool 
move_is_legal state x y dx dy =
	not(out_of_bounds destination_x destination_y)
	&& (unit_at state destination_x destination_y) == '-'
	&& (((abs dx) < 2 && (abs dy) < 2) 
		|| (is_enemy (unit_at state x y) (unit_at state jump_x jump_y)))
	where
		destination_x 	= x + dx
		destination_y 	= y + dy
		jump_x 			= x + (signum dx)
		jump_y 			= y + (signum dy)



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
			jump_x jump_y '-')
			destination_x destination_y (unit_at state x y))
			x y '-'
	| otherwise =
		(set_at (set_at state 
			destination_x destination_y (unit_at state x y))
			x y '-') 
	where
		destination_x 	= x + dx
		destination_y 	= y + dy
		jump_x 			= x + (signum dx)
		jump_y 			= y + (signum dy)