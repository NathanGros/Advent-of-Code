type direction =
	|North
	|West
	|South
	|East
;;


let rec parse l tab i =
	match l with
	|[] -> ()
	|[x] -> ()
	|t::q ->
		for j = 0 to String.length t - 1 do
			tab.(i).(j) <- String.get t j
		done;
		parse q tab (i+1)
;;


let next_coords x y dir =
	match dir with
	|North -> (x-1), y
	|West -> x, (y-1)
	|South -> (x+1), y
	|East -> x, (y+1)
;;


let print_matrix tab =
	for i = 0 to Array.length tab - 1 do
		for j = 0 to Array.length tab.(0) - 1 do
			print_string (String.make 1 (fst tab.(i).(j)));
		done;
		print_string "\n"
	done
;;


let conditions tab x y dir =
	let n = Array.length tab in
	let m = Array.length tab.(0) in
	x >= 0 && x < n && y >= 0 && y < m && (tab.(x).(y) <> ('#', Some dir))
;;


let rec light_beam tab result_tab x y dir =
	if conditions result_tab x y dir then begin
		result_tab.(x).(y) <- '#', Some dir;
		let c = tab.(x).(y) in
		match c, dir with
		|'/', North -> light_beam tab result_tab x (y+1) East;
		|'/', West -> light_beam tab result_tab (x+1) y South;
		|'/', South -> light_beam tab result_tab x (y-1) West;
		|'/', East -> light_beam tab result_tab (x-1) y North;
		|'\\', North -> light_beam tab result_tab x (y-1) West;
		|'\\', West -> light_beam tab result_tab (x-1) y North;
		|'\\', South -> light_beam tab result_tab x (y+1) East;
		|'\\', East -> light_beam tab result_tab (x+1) y South;
		|'-', North |'-', South -> light_beam tab result_tab x (y+1) East;
											light_beam tab result_tab x (y-1) West;
		|'|', West |'|', East -> light_beam tab result_tab (x+1) y South;
											light_beam tab result_tab (x-1) y North;
		|_, North -> light_beam tab result_tab (x-1) y North;
		|_, West -> light_beam tab result_tab x (y-1) West;
		|_, South -> light_beam tab result_tab (x+1) y South;
		|_, East -> light_beam tab result_tab x (y+1) East;
	end
	else ()
;;


let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let len = match input_list with |[] -> 0 |t::q -> String.length t in
	let tab = Array.make_matrix (List.length input_list - 1) len '.' in
	let result_tab = Array.make_matrix (List.length input_list - 1) len ('.', None) in
	parse input_list tab 0;
	light_beam tab result_tab 0 0 East;
	print_matrix result_tab;

	let result = ref 0 in
	for i = 0 to Array.length result_tab - 1 do
		for j = 0 to Array.length result_tab.(0) - 1 do
			match result_tab.(i).(j) with
			|'#', _ -> result := !result + 1
			|_, _ -> ()
		done
	done;
	print_int !result;
	print_string "\n"
