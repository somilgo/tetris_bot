type settings_t = {
	timebank : int;
	time_per_move : int;
	player_names : string list;
	your_bot : string;
	field_width : int;
	field_height : int;
}
let settings = ref { 
    timebank = 0;
    time_per_move = 0;
    player_names = [];
    your_bot = "";
    field_width = 0;
    field_height = 0;
}

let process_settings (l : string list) = 
	begin match l with
	| [attr; value] ->
		let _ = begin match attr with
		| "timebank" -> !settings.timebank = int_of_string value
		| "time_per_move" -> !settings.time_per_move = int_of_string value
		| "player_names" -> !settings.player_names = String.split_on_char ',' value
		| "your_bot" -> !settings.your_bot = value
		| "field_width" -> !settings.field_width = int_of_string value
		| "field_height" -> !settings.field_height = int_of_string value
		| _ -> failwith "Invalid setting attribute given"
		end in ()
	| _ -> failwith "Invalid setting given."
	end

let process_update (l : string list) = 
	begin match l with 
	| ["game"; attr; value] ->
		let _ = begin match attr with
		| "round" -> ()
		| "this_piece_type" -> ()
		| "next_piece_type" -> ()
		| "this_piece_position" -> ()
		| _ -> failwith "Invalid update attribute given with type `game`"
		end in () 
	| [player; attr; value] -> 
		let _ = begin match attr with
		| "row_points" -> ()
		| "combo" -> ()
		| "skips" -> ()
		| "field" -> ()
		| _ -> failwith "Invalid update attribute given with type `player`"
		end in ()
	| _ -> failwith "Invalid update given."
	end

let process_action (l : string list) = 
	begin match l with
	| ["move"; time] -> print_string "down"; ()
	| _ -> failwith "Invalid action given"
	end

let rec run (_ : unit) : unit = 
    let info = read_line () in
    let info_list = String.split_on_char ' ' info in 
    begin match List.hd info_list with
    | "settings" -> process_settings (List.tl info_list)
    | "update" -> process_update (List.tl info_list)
    | "action" -> process_action (List.tl info_list)
    | _ -> failwith "Invalid input type given."
	end;
    run ()

let _ = run ();;

