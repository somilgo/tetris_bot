let fitness (game : Game.t) : float = 
	let open Game in
    remove_piece game;
    draw_piece game;
    let out = 
    	((-. 1.0) *. compute_height game) +. 
    	((+. 1.0) *. compute_lines game)  +.
    	((-. 1.5) *. compute_holes game)
    in
    remove_piece game;
    out