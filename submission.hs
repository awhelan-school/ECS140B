import Data.List



capture :: [String] -> Char -> Int = 
capture previous control depth = 
	minimax previous control depth h_combo


-- heuristics
h_pawn_count :: [String] -> Char -> Int
h_pawn_count previous control 
	| is_win previous control control 	= 100000
	| is_win previous enemy control	= -100000 
	| otherwise 				= (count_chars state control)
	where
		state = (head previous)
		enemy = opposite control

safe_index :: Char -> String -> Int
safe_index c str = case (elemIndex c str) of
    Nothing   -> (-1)
    Just val  -> val

h_flag_y :: [String] -> Char -> Int
h_flag_y previous control 
	| is_win previous control control 	= 100000
	| is_win previous enemy control 	= -100000 
	| control == 'w' 			= get_y (safe_index 'W' state) state
	| otherwise					= get_y (safe_index 'B' (reverse state)) state
	where
		state = (head previous)
		enemy = opposite control

h_mobility previous control
	| is_win previous control control 	= 100000
	| is_win previous enemy control 	= -100000 
	| otherwise = length (all_moves previous control)
	where
		enemy = opposite control

h_combo previous control
	| is_win previous control control 	= 100000
	| is_win previous enemy control 	= -100000 
	| otherwise = 
		4 * (h_pawn_count previous control) + 
		3 * (h_flag_y previous control) + 
		3 * (h_mobility previous control)
	where
		enemy = opposite control



-- returns opponent of player
-- args: controlling player
opposite :: Char -> Char
opposite control
	| control == 'w' = 'b'
	| otherwise 	 = 'w'

-- returns number of pawns of a faction
-- args: current state, currently controlling player
num_pawns :: String -> Char -> Int
num_pawns state control
	| control == 'w' 	= (count_chars state 'w') 
	| otherwise 		= (count_chars state 'b') 



-- returns flag character for given player
-- args: currently controlling player
flag :: Char -> Char
flag control
	| control == 'w' = 'W'
	| otherwise 	 = 'B'

-- returns true if flag reached opposite end of board
-- args: current state, currently controlling player
flag_reached_end :: String -> Char -> Bool
flag_reached_end state control
	| control == 'w' = elem 'W' (drop (width * (width - 1)) state)
	| otherwise		 = elem 'B' (take width state)
	where 
		width = floor (sqrt (fromIntegral (length state)))

-- returns true if flag passed all enemy pawns
-- args: previous states, currently controlling player
flag_passed_pawns :: String -> Char -> Bool
flag_passed_pawns state control
	| control == 'w' = flag_passed_pawns_w state
	| otherwise		 = flag_passed_pawns_b state
flag_passed_pawns_w :: String -> Bool
flag_passed_pawns_w state =
	not (elem 'b' (drop ((get_y (safe_index 'W' state) state) * width) state))
	where
		width = floor (sqrt (fromIntegral (length state)))
flag_passed_pawns_b :: String -> Bool
flag_passed_pawns_b state =
	not (elem 'w' (drop ((get_y (safe_index 'B' reversed) reversed) * width) reversed))
	where
		reversed = reverse state
		width = floor (sqrt (fromIntegral (length state)))
	

-- returns true if current state is a win for given player
-- args: previous states, examined player, currently controlling player
is_win :: [String] -> Char -> Char -> Bool
is_win previous control current_control = 
	(num_pawns state enemy) == 0 --captured all pawns
	|| (count_chars state (flag enemy)) == 0 --captured flag
	|| (flag_passed_pawns state control)
	|| (control /= current_control && (null (all_moves previous current_control))) --no possible moves for player whose it is now
	where 
		state = (head previous)
		enemy = opposite control

-- selects best move/resulting state based on minimax and a heuristic
-- args: previous states, controlling player, maximum depth, heuristic function to use
minimax :: [String] -> Char -> Int -> ([String] -> Char -> Int) -> String
minimax previous control depth heuristic
	| depth == 0 = head previous
	| otherwise  = 
		fst	(maximumBy compare_states (map 
			(\new_state -> minimax_helper (new_state:previous) (opposite control) control (depth - 1) heuristic) 
			(all_moves previous control)))

minimax_helper :: [String] -> Char -> Char -> Int -> ([String] -> Char -> Int) -> (String, Int)
minimax_helper previous control maximizing_control depth heuristic
	| (depth == 0) || (is_win previous control control) || (is_win previous (opposite control) control) = 
		(head previous, heuristic previous maximizing_control) --reached max depth or a win
	| control == maximizing_control 				=
		(head previous, snd max) 
	| otherwise			 							= 
		(head previous, snd min) 
	where 
		max = maximumBy compare_states (map 
			(\new_state -> minimax_helper (new_state:previous) (opposite control) maximizing_control (depth - 1) heuristic) 
			(all_moves previous control))
		min = minimumBy compare_states (map 
			(\new_state -> minimax_helper (new_state:previous) (opposite control) maximizing_control (depth - 1) heuristic) 
			(all_moves previous control))

-- compare a tuple of state-score based on score
-- args: two state-score tuples
compare_states :: (String, Int) -> (String, Int) -> Ordering
compare_states x y
	| (snd x) < (snd y)  = LT
	| (snd x) == (snd y) = EQ
	| otherwise			 = GT


-- counts chars in string
-- args: string, char to count
count_chars :: String -> Char -> Int
count_chars str c = length $ filter (== c) str


-- returns y coordinate of position
-- args: z coordinate, state of the board
get_y :: Int -> String -> Int
get_y z state = 
	div z width
	where
		width = floor (sqrt (fromIntegral (length state)))


-- returns x coordinate of position
-- args: z coordinate, state of the board
get_x :: Int -> String -> Int
get_x z state = 
	mod z width
	where
		width = floor (sqrt (fromIntegral (length state)))

-- returns "z" coordinate of position, 0 to length of state string
-- args: position, state of the board
get_z :: Int -> Int -> String -> Int
get_z x y state = 
	x + (y * width)
	where
		width = floor (sqrt (fromIntegral (length state)))

-- returns unit char at position
-- args: position, state of the board
unit_at :: String -> Int -> Int -> Char
unit_at state x y 
	| (out_of_bounds x y state) = 'E' 
	| otherwise 		= state !! (get_z x y state)

-- true if position is outside of the board
-- args: position, state of the board
out_of_bounds :: Int -> Int -> String -> Bool
out_of_bounds x y state = 
	0 > x || x >= width || 0 > y || y >= width
	where
		width = floor (sqrt (fromIntegral (length state)))


-- need separate is_enemy and my_unit and not one function that is negated because
-- need to distinguish between me, enemy and blank spaces '-'

-- true if unit belongs to opposite faction
-- args: my faction('w' or 'b'), unit char
is_enemy :: Char -> Char -> Bool
is_enemy me other
	| (me == 'w' || me == 'W') = (other == 'b' || other == 'B')
	| (me == 'b' || me == 'B') = (other == 'w' || other == 'W')
	| otherwise 			   = False

-- true if unit belongs is same faction
-- args: my faction('w' or 'b'), unit char
my_unit :: Char -> Char -> Bool
my_unit me unit
	| (me == 'w' || me == 'W') = (unit == 'w' || unit == 'W')
	| (me == 'b' || me == 'B') = (unit == 'b' || unit == 'B')
	| otherwise 			   = False





-- get dx/dy for unit(flag or pawn)
-- args: unit char, index of the displacement, pawns have 6 moves, flags have 4
get_dx :: Char -> Int -> Int
get_dx unit i
	| (unit == 'w' || unit == 'b') = pawn_dx unit i 
	| (unit == 'W' || unit == 'B') = flag_dx unit i 

get_dy :: Char -> Int -> Int
get_dy unit i
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

-- forward direction for this control
-- arg: controlling player ('w' or 'b')
forward :: Char -> Int
forward control
	| control == 'w' = 1
	| control == 'b' = -1



-- go through board, get all pieces of same faction
-- get possible moves for them
-- substract previous states (\\)
-- args: list of previous states, char for controlling player('w' or 'b')
all_moves :: [String] -> Char -> [String]
all_moves previous control =
	concat (map (\z -> moves previous control z) [0..((length state) - 1)])
	where
		state = head previous


-- get a list of possible moves for a unit(as strings of resulting states)
-- 6 moves for pawns, 4 moves for flags
-- args: list of previous states, char for controlling player('w' or 'b'), current state, z coordinate of unit
moves :: [String] -> Char -> Int -> [String]
moves previous control z
	| (my_unit control unit) && (unit == 'w' || unit == 'b') 	=
		(moves_helper state (get_x z state) (get_y z state) 5) \\ previous
	| (my_unit control unit)									=
		(moves_helper state (get_x z state) (get_y z state) 3) \\ previous
	| otherwise 												= []
	where 
		state = head previous
		unit = unit_at state (get_x z state) (get_y z state)

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
-- args: current state, position of moved unit, displacement of moved unit
move_is_legal :: String -> Int -> Int -> Int -> Int -> Bool 
move_is_legal state x y dx dy =
	not(out_of_bounds destination_x destination_y state)
	&& (unit_at state destination_x destination_y) == '-'
	&& (((abs dx) < 2 && (abs dy) < 2) 
		|| (is_enemy (unit_at state x y) (unit_at state jump_x jump_y)))
	where
		destination_x 	= x + dx
		destination_y 	= y + dy
		jump_x 			= x + (signum dx)
		jump_y 			= y + (signum dy)


-- sets unit at position in state
-- args: current state, position to set at, unit to set
set_at :: String -> Int -> Int -> Char -> String
set_at state x y unit =
	take (get_z x y state) state ++ [unit] ++ drop ((get_z x y state) + 1) state


-- erase unit in front if jumping
-- draw unit at new position
-- erase unit at old position
-- args: current state, position of moved unit, displacement of moved unit
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












