type settings_t = {
	timebank : int;
	time_per_move : int;
	player_names : string list;
	your_bot : string;
	field_width : int;
	field_height : int;
}

type t = {
	settings : settings_t;
	field : int array array;
	curr_piece : Piece.t;
	next_piece : Piece.t
}

val init_settings : settings_t 

val init_game : t

val update_settings : t -> string -> string -> t

val update_field : t -> string -> t

val update_piece : t -> string -> t

val update_next_piece : t -> string -> t