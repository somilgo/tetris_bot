type t = 
	| Unknown
	| I 
	| J 
	| L
	| O 
	| S
	| T 
	| Z

let get_piece_type (value : string) : t = 
	begin match value with 
	| "I" -> I
	| "J" -> J
	| "L" -> L
	| "O" -> O
	| "S" -> S
	| "T" -> T
	| "Z" -> Z
	| _ -> failwith "Invalid piece type."
	end

let get_piece_arr (piece : t) : int array array = 
	let piece_list = begin match piece with
	| Unknown -> failwith "Invalid piece type."
	| I -> [
			[0; 0; 0; 0];
			[1; 1; 1; 1];
			[0; 0; 0; 0];
			[0; 0; 0; 0];
		]
	| J -> [
			[1; 0; 0];
			[1; 1; 1];
			[0; 0; 0];
		]
	| L -> [
			[0; 0; 1];
			[1; 1; 1];
			[0; 0; 0];
		]
	| O -> [
			[1; 1];
			[1; 1];
		]
	| S -> [
			[0; 1; 1];
			[1; 1; 0];
			[0; 0; 0];
		]
	| T -> [
			[0; 1; 0];
			[1; 1; 1];
			[0; 0; 0];
		]
	| Z -> [
			[1; 1; 0];
			[0; 1; 1];
			[0; 0; 0];
		]
	end in
	Array.of_list (List.map Array.of_list piece_list)

let rotate_right (piece_arr : int array array) = 
	let n = Array.length piece_arr in
	let transpose = Array.init n (fun i -> 
		Array.init n (fun j -> piece_arr. (j). (i))
	) in
	Array.map (fun row -> Array.of_list (List.rev (Array.to_list row))) transpose

let rec rotate (n : int) (piece_arr : int array array) = 
	if n <= 0 then piece_arr else rotate (n-1) (rotate_right piece_arr)

let rotate_left (piece_arr : int array array) = 
	rotate 3 piece_arr