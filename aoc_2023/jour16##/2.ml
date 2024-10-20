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


let lighten tab n m x y dir =
	let result_tab = Array.make_matrix n m ('.', None) in
	light_beam tab result_tab x y dir;
	let light_nb = ref 0 in
	for i = 0 to n - 1 do
		for j = 0 to m - 1 do
			match result_tab.(i).(j) with
			|'#', _ -> light_nb := !light_nb + 1
			|_, _ -> ()
		done
	done;
	!light_nb
;;
	

let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let len = match input_list with |[] -> 0 |t::q -> String.length t in
	let tab = Array.make_matrix (List.length input_list - 1) len '.' in
	parse input_list tab 0;
	let n = List.length input_list - 1 in
	let m = len in

	let result = ref 0 in
	let nb = ref 0 in
	let xfinal = ref 0 in
	let yfinal = ref 0 in

	nb := lighten tab n m 0 0 East;
	if !nb > !result then
		result := !nb;
	nb := lighten tab n m 0 0 South;
	if !nb > !result then
		result := !nb;
	nb := lighten tab n m 0 (m-1) West;
	if !nb > !result then
		result := !nb;
	nb := lighten tab n m 0 (m-1) South;
	if !nb > !result then
		result := !nb;
	nb := lighten tab n m (n-1) 0 East;
	if !nb > !result then
		result := !nb;
	nb := lighten tab n m (n-1) 0 North;
	if !nb > !result then
		result := !nb;
	nb := lighten tab n m (n-1) (m-1) West;
	if !nb > !result then
		result := !nb;
	nb := lighten tab n m (n-1) (m-1) North;
	if !nb > !result then
		result := !nb;
	
	for i = 1 to m-2 do
		nb := lighten tab n m 0 i South;
		if !nb > !result then begin
			result := !nb;
			yfinal := i;
			xfinal := 0
		end
	done;
	for i = 1 to m-2 do
		nb := lighten tab n m (n-1) i North;
		if !nb > !result then begin
			result := !nb;
			yfinal := i;
			xfinal := (n-1)
		end
	done;
	for i = 1 to n-2 do
		nb := lighten tab n m i 0 East;
		if !nb > !result then begin
			result := !nb;
			xfinal := i;
			yfinal := 0
		end
	done;
	for i = 1 to n-2 do
		nb := lighten tab n m i (m-1) West;
		if !nb > !result then begin
			result := !nb;
			xfinal := i;
			yfinal := (m-1)
		end
	done;
		
	

	let result_tab = Array.make_matrix n m ('.', None) in
	light_beam tab result_tab !xfinal !yfinal West;
	print_matrix result_tab;
	print_int !result;
	print_string "\n"
