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

let init_settings = { 
    timebank = 0;
    time_per_move = 0;
    player_names = [];
    your_bot = "";
    field_width = 0;
    field_height = 0;
} 

let init_game = 
	let open Piece in 
	{
		settings = init_settings;
		field = Array.make 0 (Array.make 0 0);
		curr_piece = Unknown;
		next_piece = Unknown
	}

let update_settings (game : t) (attr : string) (value : string) = 
	let new_settings = 
		begin match attr with
		| "timebank" -> {game.settings with timebank = int_of_string value}
		| "time_per_move" -> {game.settings with time_per_move = int_of_string value}
		| "player_names" -> {game.settings with player_names = String.split_on_char ',' value}
		| "your_bot" -> {game.settings with your_bot = value}
		| "field_width" -> {game.settings with field_width = int_of_string value}
		| "field_height" -> {game.settings with field_height = int_of_string value}
		| _ -> failwith "Invalid setting attribute given"
		end in
	{game with settings = new_settings}

let update_field (game : t) (value : string) = 
	let rows = String.split_on_char ';' value in
	let rows_l = List.map (fun row -> 
		let row_l = String.split_on_char ',' row in
		let row_l_int = List.map int_of_string row_l in 
		Array.of_list row_l_int
	) rows in
	let new_field = Array.of_list rows_l in
	{game with field = new_field}

let update_piece (game : t) (value : string) = 
	{game with curr_piece = Piece.get_piece_type value}

let update_next_piece (game : t) (value : string) = 
	{game with next_piece = Piece.get_piece_type value}

let check_collision (game : t) (piece_arr : int array array) (x : int) (y : int) = 
	let field = game.field in
	let coll_arr = Array.mapi (fun i r -> 
		Array.fold_left (fun (n, b) e ->
			let newb = b || 
				i >= game.settings.field_height ||
				n >= game.settings.field_width ||
				(e > 0 && (field. (x + i). (y + n) > 1)) in
			(n+1, newb)
		) (0, false) r
	) piece_arr in
	Array.fold_left (fun acc (_, b) ->
		acc || b
	) false coll_arr