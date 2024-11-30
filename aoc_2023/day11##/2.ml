let parse l =
	let n = List.length l - 1 in
	let m = match l with |[] -> 0 |t::q -> String.length t in
	let tab = Array.make_matrix n m ' ' in
	let rec aux l tab i =
		match l with
		|[] -> ()
		|[x] -> ()
		|t::q ->
			for j = 0 to String.length t - 1 do
				tab.(i).(j) <- String.get t j
			done;
			aux q tab (i+1)
	in
	aux l tab 0;
	tab
;;


let list_to_array l =
	let n = List.length l in
	let x0 = match l with |t::q -> t |_ -> failwith "void_list" in
	let tab = Array.make n x0 in
	let rec aux tab l i =
		match l with
		|[] -> ()
		|t::q ->
			tab.(i) <- t;
			aux tab q (i+1)
	in
	aux tab l 0;
	tab
;;


let print_tab tab =
	let n = Array.length tab in
	let m = Array.length tab.(0) in
	for i = 0 to n - 1 do
		for j = 0 to m - 1 do
			print_string (String.make 1 tab.(i).(j))
		done;
		print_string "\n"
	done
;;


let is_void_line tab n i =
	let flag = ref true in
	for j = 0 to n - 1 do
		if tab.(i).(j) = '#' then flag := false
	done;
	!flag
;;


let is_void_column tab m j =
	let flag = ref true in
	for i = 0 to m - 1 do
		if tab.(i).(j) = '#' then flag := false
	done;
	!flag
;;


let expand tab =
	let m = Array.length tab in
	let n = Array.length tab.(0) in
	for i = 0 to m - 1 do
		if is_void_line tab m i then
			for j = 0 to n - 1 do
				tab.(i).(j) <- '@'
			done
	done;
	for j = 0 to n - 1 do
		if is_void_column tab n j then
			for i = 0 to m - 1 do
				tab.(i).(j) <- '@'
			done
	done
;;


let galaxy_coords_list tab =
	let m = Array.length tab in
	let n = Array.length tab.(0) in
	let l = ref [] in
	for i = 0 to m - 1 do
		for j = 0 to n - 1 do
			if tab.(i).(j) = '#' then l := !l @ [(i, j)]
		done
	done;
	!l
;;


let distance_manhattan tab (x1, y1) (x2, y2) =
	let max_x = max x1 x2 in
	let min_x = min x1 x2 in
	let max_y = max y1 y2 in
	let min_y = min y1 y2 in
	let res = ref 0 in
	for i = min_x to max_x - 1 do
		if tab.(i).(min_y) <> '@' then
			res := !res + 1
		else res := !res + 1000000
	done;
	for j = min_y to max_y - 1 do
		if tab.(min_x).(j) <> '@' then
			res := !res + 1
		else res := !res + 1000000
	done;
	!res
;;


let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	
	let tab = parse input_list in
	expand tab;
	print_tab tab;
	print_string "\n";

	let galaxy_coords = list_to_array (galaxy_coords_list tab) in

	let result = ref 0 in
	for i = 0 to (Array.length galaxy_coords) - 1 do
		for j = i to (Array.length galaxy_coords) - 1 do
			result := !result + (distance_manhattan tab galaxy_coords.(i) galaxy_coords.(j))
		done
	done;

	print_int !result;
	print_string "\n"
