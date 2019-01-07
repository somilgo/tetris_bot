let rec move_rightmost (game : Game.t) = 
	let game', res = Game.move_right game in
	if res then move_rightmost game'
	else game'

let rec move_bottommost (game : Game.t) = 
	let game', res = Game.move_down game in
	if res then move_bottommost game'
	else game'

let rec find_best (game : Game.t) (best_fitness : float) (best_game : Game.t) : Game.t * float = 
	let temp_game = move_bottommost game in
	let fitness = Game.fitness temp_game in
	let game', res = Game.move_left game in

	if fitness > best_fitness then
		if res then find_best game' fitness temp_game
		else (temp_game, fitness)
	else 
		if res then find_best game' best_fitness best_game
		else (best_game, best_fitness)

let make_move (game : Game.t) = 
	let best_games = List.map (fun r -> 
		let game', _ = Game.rotate_piece r game in
		let game'' = move_rightmost game' in 
		(find_best game'' min_float game'')
	) [0;1;2;3] in 
	let best_game = List.fold_left (fun (bg, bf) (g, f) ->
		if f > bf then (g, f) else (bg, bf)
	) (List.hd best_games) best_games in 
	Game.print_moves (fst best_game)