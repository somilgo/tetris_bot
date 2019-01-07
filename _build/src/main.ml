let process_settings (game : Game.t) (l : string list)  : Game.t = 
	begin match l with
	| [attr; value] ->
		Game.update_settings game attr value
	| _ -> failwith "Invalid setting given."
	end


let process_update (game : Game.t) (l : string list) : Game.t = 
	let open Game in 
	begin match l with 
	| ["game"; attr; value] ->
		begin match attr with
		| "round" -> game
		| "this_piece_type" -> Game.update_piece game value
		| "next_piece_type" -> Game.update_next_piece game value
		| "this_piece_position" -> Game.update_piece_pos game value
		| _ -> failwith "Invalid update attribute given with type `game`"
		end
	| [player; attr; value] -> 
		begin match attr with
		| "row_points" -> game
		| "combo" -> game
		| "skips" -> game
		| "field" -> if player = game.settings.your_bot then Game.update_field game value
					 else game
		| _ -> failwith "Invalid update attribute given with type `player`"
		end
	| _ -> failwith "Invalid update given."
	end

let process_action (game : Game.t) (l : string list) = 
	begin match l with
	| ["move"; time] -> Bot.make_move game
	| _ -> failwith "Invalid action given"
	end

let rec run (game : Game.t) : unit = 
    let info = read_line () in
    let info_list = String.split_on_char ' ' info in 
    let new_game = begin match List.hd info_list with
    | "settings" -> process_settings game (List.tl info_list)
    | "update" -> process_update game (List.tl info_list)
    | "action" -> process_action game (List.tl info_list); game
    | _ -> failwith "Invalid input type given."
	end in 
    run new_game

let () = run Game.init_game;;

