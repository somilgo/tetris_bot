type t = 
	| Unknown
	| I of int
	| J of int
	| L of int
	| O of int
	| S of int
	| T of int 
	| Z of int

let get_piece_type (value : string) : t = 
	begin match value with 
	| "I" -> I 0
	| "J" -> J 0
	| "L" -> L 0
	| "O" -> O 0
	| "S" -> S 0
	| "T" -> T 0
	| "Z" -> Z 0
	| _ -> failwith "Invalid piece type."
	end

let get_rot_no (piece : t) = 
	begin match piece with
	| Unknown -> failwith "Invalid piece type"
	| I x -> x
	| J x -> x
	| L x -> x
	| O x -> x
	| S x -> x
	| T x -> x 
	| Z x -> x
	end

let rotate (n : int) (piece : t) = 
	begin match piece with
	| Unknown -> failwith "Invalid piece type"
	| I x -> I (x+n mod 4)
	| J x -> J (x+n mod 4)
	| L x -> L (x+n mod 4)
	| O x -> O (x+n mod 4)
	| S x -> S (x+n mod 4)
	| T x -> T (x+n mod 4) 
	| Z x -> Z (x+n mod 4)
	end

let unknown_piece = Unknown

let rotate_right (piece_arr : int array array) = 
	let n = Array.length piece_arr in
	let transpose = Array.init n (fun i -> 
		Array.init n (fun j -> piece_arr. (j). (i))
	) in
	Array.map (fun row -> Array.of_list (List.rev (Array.to_list row))) transpose

let rec rotate_piece_arr (n : int) (piece_arr : int array array) = 
	if n <= 0 then piece_arr else rotate_piece_arr (n-1) (rotate_right piece_arr)

let rotate_left (piece_arr : int array array) = 
	rotate_piece_arr 3 piece_arr

let get_piece_arr (piece : t) : int array array = 
	let piece_list, rot = begin match piece with
	| Unknown -> failwith "Invalid piece type."
	| I r -> ([
			[0; 0; 0; 0];
			[1; 1; 1; 1];
			[0; 0; 0; 0];
			[0; 0; 0; 0];
		], r)
	| J r -> ([
			[1; 0; 0];
			[1; 1; 1];
			[0; 0; 0];
		], r)
	| L r -> ([
			[0; 0; 1];
			[1; 1; 1];
			[0; 0; 0];
		], r)
	| O r -> ([
			[1; 1];
			[1; 1];
		], r)
	| S r -> ([
			[0; 1; 1];
			[1; 1; 0];
			[0; 0; 0];
		], r)
	| T r -> ([
			[0; 1; 0];
			[1; 1; 1];
			[0; 0; 0];
		], r)
	| Z r -> ([
			[1; 1; 0];
			[0; 1; 1];
			[0; 0; 0];
		], r)
	end in
	rotate_piece_arr rot (Array.of_list (List.map Array.of_list piece_list))