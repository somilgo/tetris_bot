type settings_t = {
	mutable timebank : int;
	mutable time_per_move : int;
	mutable player_names : string list;
	mutable your_bot : string;
	mutable field_width : int;
	mutable field_height : int;
}
val settings : settings_t ref