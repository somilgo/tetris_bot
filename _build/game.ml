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
		next_piece = Unknown;
		piece_pos_x = 3;
		piece_pos_y = -1;
		init_piece_pos_x = 3;
		init_piece_pos_y = -1;
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

let update_piece_pos_lit (game : t) (x : int) (y : int) = 
	{game with piece_pos_x=x; piece_pos_y=y;
			   init_piece_pos_x=x; init_piece_pos_y=y;}

let update_piece_pos (game : t) (value : string) = 
	let coords = String.split_on_char ',' value in
	begin match coords with
	| [x; y] -> update_piece_pos_lit game (int_of_string x) (int_of_string y)
	| _ -> failwith "Invalid piece position supplied."
	end

let check_collision (game : t) (piece_arr : int array array) (x : int) (y : int) = 
	let field = game.field in
	let coll_arr = Array.mapi (fun i r -> 
		Array.fold_left (fun (n, b) e ->
			let newb = b || (e = 1 &&
				(x+n >= game.settings.field_width ||
				y+i >= game.settings.field_height ||
				x+n < 0)) in
			let newb' = if newb || (y + i) < 0 then newb else
			(e > 0 && (field. (y + i). (x + n) > 1)) in
			(n+1, newb')
			
		) (0, false) r
	) piece_arr in
	Array.fold_left (fun acc (_, b) ->
		acc || b
	) false coll_arr

let remove_piece (game : t) = 
	let board = game.field in
	Array.iteri (fun y r -> 
		Array.iteri (fun x e -> 
			if e = 1 then (board. (y). (x) <- 0)
		) r
	) board 

let draw_piece (game : t) = 
	let board = game.field in 
	let piece = Piece.get_piece_arr game.curr_piece in 
	let currx = game.piece_pos_x in 
	let curry = game.piece_pos_y in 

	Array.iteri (fun y r -> 
		Array.iteri (fun x e -> 
			if e = 1 && (y + curry) >= 0 && (y + curry) < game.settings.field_height && 
			   (x + currx) >= 0 && (x + currx) < game.settings.field_width
			then board. (y+curry). (x+currx) <- e
			else ()
		) r
	) piece

let print_board (game : t) =
	remove_piece game;
	draw_piece game;

	Array.iter (fun r ->
		Array.iter (fun e -> 
			prerr_string ((string_of_int e) ^ " ")
		) r;
		prerr_endline " "
	) game.field;
	prerr_endline "-------------";
	remove_piece game

let rotate_piece (n : int) (game : t) = 
	let piece' = Piece.rotate n game.curr_piece in 
	let piece = Piece.get_piece_arr piece' in 
	let game' = {game with curr_piece = piece'} in 
	let currx = game.piece_pos_x in 
	let curry = game.piece_pos_y in 
	let coll = check_collision game piece (currx) (curry) in 
	if coll then (game, false)
	else (game', true)

let move_left (game : t) = 
	let piece = Piece.get_piece_arr game.curr_piece in 
	let currx = game.piece_pos_x in 
	let curry = game.piece_pos_y in 
	let coll = check_collision game piece (currx-1) (curry) in 
	if coll then (game, false)
	else ({game with piece_pos_x=currx-1; piece_pos_y=curry}, true)

let move_right (game : t) = 
	let piece = Piece.get_piece_arr game.curr_piece in 
	let currx = game.piece_pos_x in 
	let curry = game.piece_pos_y in 
	let coll = check_collision game piece (currx+1) (curry) in 
	if coll then (game, false)
	else ({game with piece_pos_x=currx+1; piece_pos_y=curry}, true)

let move_down (game : t) = 
	let piece = Piece.get_piece_arr game.curr_piece in 
	let currx = game.piece_pos_x in 
	let curry = game.piece_pos_y in 
	let coll = check_collision game piece (currx) (curry+1) in 
	if coll then (game, false)
	else ({game with piece_pos_x=currx; piece_pos_y=curry+1}, true)

let rec make_move_list (n : int) (move_f : int -> 'a) : 'a list =
	if n = 0 then [] else
	(make_move_list (n-1) move_f) @ [move_f (n-1)]

let print_moves (game : t) = 
	let piece = game.curr_piece in 
	let currx = game.piece_pos_x in 
	let initx = game.init_piece_pos_x in
	let offset = initx - currx in 

	let move_list = if offset > 0 then make_move_list offset (fun _ -> "left")
	else make_move_list (-offset) (fun _ -> "right") in
	let rot_list = make_move_list (Piece.get_rot_no piece) (fun _ -> "turnright") in 
	prerr_endline (
		String.concat "," (rot_list @ move_list)
	);
	print_endline (
		String.concat "," ((rot_list @ move_list) @ ["drop"])
	)

let compute_height (game : t) : float = 
	let bit_game = Array.map (fun r -> 
		Array.fold_left (fun b x -> 
			if (b = 1) || (x >= 1) then 1 else 0
		) 0 r
	) game.field in 
	float (Array.fold_left (fun sum i -> sum+i) 0 bit_game)

let compute_agg_height (game : t) : float = 
	let heights = Array.init game.settings.field_width (fun _ -> 0) in
	let _ = Array.iteri (fun y r -> 
		Array.iteri (fun x e -> 
			let curr_h = game.settings.field_height - y in 
			if e > 0 && heights. (x) = 0 then heights. (x) <- curr_h
		) r
	) game.field in
	let agg_height = Array.fold_left (fun tot h ->
		tot + h
	) 0 heights in 
	float agg_height

let compute_holes (game : t) : float = 
	let bit_hole = Array.init game.settings.field_width (fun _ -> false) in
	let hole_counter = Array.fold_left (fun hole_count r -> 
		let bit_row = Array.mapi (fun x e ->

			if bit_hole. (x) then
				(if e = 0 then 1 else 0)
			else 
				(if e > 0 then bit_hole. (x) <- true; 0)
		) r in
		let inc_count = Array.fold_left (fun row_count i ->
			row_count + i
		) 0 bit_row in
		hole_count + inc_count
	) 0 game.field in
	float hole_counter

let compute_lines (game : t) : float = 
	let lines = Array.fold_left (fun line_count r -> 
		let count_inc = Array.fold_left (fun line e ->
			if e > 0 then line else 0
		) 1 r in
		line_count + count_inc
	) 0 game.field in
	float lines

let compute_bumpy (game : t) : float = 
	let heights = Array.init game.settings.field_width (fun _ -> 0) in
	let _ = Array.iteri (fun y r -> 
		Array.iteri (fun x e -> 
			let curr_h = game.settings.field_height - y in 
			if e > 0 && heights. (x) = 0 then heights. (x) <- curr_h
		) r
	) game.field in
	let bumpy = Array.fold_left (fun (prev, bsum) h ->
		(h, bsum + (abs (prev - h)))
	) (heights. (0), 0) heights in
	float (snd bumpy) 
	(* let normalized_bumpy = (float (snd bumpy)) /. (float (game.settings.field_width - 1)) in
	normalized_bumpy *)