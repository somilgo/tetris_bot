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
	next_piece : Piece.t;
	piece_pos_x : int;
	piece_pos_y : int;
	init_piece_pos_x : int;
	init_piece_pos_y : int;
}

val init_settings : settings_t 

val init_game : t

val update_settings : t -> string -> string -> t

val update_field : t -> string -> t

val update_piece : t -> string -> t

val update_next_piece : t -> string -> t

val update_piece_pos : t -> string -> t

val move_left : t -> (t * bool)

val move_right : t -> (t * bool)

val move_down : t -> (t * bool)

val rotate_piece : int -> t -> (t * bool)

val fitness : t -> float

val print_board : t -> unit

val print_moves : t -> unit