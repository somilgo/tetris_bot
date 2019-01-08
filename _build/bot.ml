let rec move_rightmost (game : Game.t) = 
	let game', res = Game.move_right game in
	if res then move_rightmost game'
	else game'

let rec move_bottommost (game : Game.t) = 
	let game', res = Game.move_down game in
	if res then move_bottommost game'
	else game'

let rec best_search (game : Game.t) (best_fitness : float) (best_game : Game.t)
					(fit_fun : Game.t -> float) : Game.t * float = 
	let temp_game = move_bottommost game in
	let fitness = fit_fun temp_game in
	let game', res = Game.move_left game in
	if fitness > best_fitness then
		if res then best_search game' fitness temp_game fit_fun
		else (temp_game, fitness)
	else 
		if res then best_search game' best_fitness best_game fit_fun
		else (best_game, best_fitness)

let find_best (game : Game.t) (fit_fun : Game.t -> float) : Game.t * float = 
	let best_games = List.map (fun r -> 
		let game', _ = Game.rotate_piece r game in
		let game'' = move_rightmost game' in 
		(best_search game'' (-. max_float) game'' fit_fun)
	) [0;1;2;3] in 
	let best_game = List.fold_left (fun (bg, bf) (g, f) ->
		if f > bf then (g, f) else (bg, bf)
	) (List.hd best_games) best_games in 
	best_game

let fitness (game : Game.t) : float = 
	let open Game in 
    remove_piece game;
    draw_piece game;
    let height = compute_agg_height game in 
    let lines = compute_lines game in 
    let out = 
    	((-. 1.0) *. height) +.
    	((-. 1.0) *. compute_bumpy game) +.  
    	(1.0 *. lines ** 2.0)  +.
    	((-. 2.0) *. compute_holes game)
    in
    remove_piece game;
    out

let lookahead_fitness (game : Game.t) : float = 
    let open Game in 
    remove_piece game;
    draw_piece game;
    if game.next_piece = Piece.unknown_piece then 0. else
    let solid_board = Array.map (fun r ->
        Array.map (fun e ->
            if e = 1 then 2 else e
        ) r
    ) game.field in 
    let lookahead_game = {game with 
        curr_piece = game.next_piece;
        next_piece = Piece.unknown_piece;
        field = solid_board;
        piece_pos_x = 3;
        piece_pos_y = -1;
        init_piece_pos_x = 3;
        init_piece_pos_y = -1;
    } in
    let bg, best_fitness = find_best lookahead_game fitness in 
    
    best_fitness

let aggregate_fitness_function (game : Game.t) : float = 
	(lookahead_fitness game)

let make_move (game : Game.t) = 
	let best_game = find_best game aggregate_fitness_function in 
	Game.print_moves (fst best_game)
