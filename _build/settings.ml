type settings_t = {
	mutable timebank : int;
	mutable time_per_move : int;
	mutable player_names : string list;
	mutable your_bot : string;
	mutable field_width : int;
	mutable field_height : int;
}
let settings : settings_t ref = ref { 
    timebank = 0;
    time_per_move = 0;
    player_names = [];
    your_bot = "";
    field_width = 0;
    field_height = 0;
} 