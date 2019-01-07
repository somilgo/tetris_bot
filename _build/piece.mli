type t = 
	| Unknown
	| I of int
	| J of int
	| L of int
	| O of int
	| S of int
	| T of int 
	| Z of int

val rotate : int -> t -> t

val get_piece_type : string -> t

val get_piece_arr : t -> int array array

val get_rot_no : t -> int

val unknown_piece : t