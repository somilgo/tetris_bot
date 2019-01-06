type t = 
	| Unknown
	| I 
	| J 
	| L
	| O 
	| S
	| T 
	| Z

val get_piece_type : string -> t