let rec parse l tab i =
	match l with
	[] -> ()
	|[x] -> ()
	|t::q ->
		for j = 0 to String.length t - 1 do
			tab.(i).(j) <- String.get t j
		done;
		parse q tab (i+1)
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


let void_lines tab m n =
	let flag = ref true in
	let vl = ref 0 in
	for i = 0 to m - 1 do
		flag := true;
		for j = 0 to n - 1 do
			if tab.(i).(j) = '#' then flag := false
		done;
		if !flag then vl := !vl + 1
	done;
	!vl
;;


let void_columns tab m n =
	let flag = ref true in
	let vc = ref 0 in
	for j = 0 to n - 1 do
		flag := true;
		for i = 0 to m - 1 do
			if tab.(i).(j) = '#' then flag := false
		done;
		if !flag then vc := !vc + 1
	done;
	!vc
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


let expand tab1 tab2 m n =
	let skipped_l = ref 0 in
	for i = 0 to m - 1 do
		let skipped_c = ref 0 in
		for j = 0 to n - 1 do
			tab2.(i + !skipped_l).(j + !skipped_c) <- tab1.(i).(j);
			if is_void_column tab1 m j then
				skipped_c := !skipped_c + 1
		done;
		if is_void_line tab1 n i then
			skipped_l := !skipped_l + 1
	done
;;


let galaxy_coords_list tab m n =
	let l = ref [] in
	for i = 0 to m - 1 do
		for j = 0 to n - 1 do
			if tab.(i).(j) = '#' then l := !l @ [(i, j)]
		done
	done;
	!l
;;


let distance_manhattan (x1, y1) (x2, y2) =
	if x1 < x2 then
		if y1 < y2 then
			(x2 - x1) + (y2 - y1)
		else
			(x2 - x1) + (y1 - y2)
	else
		if y1 < y2 then
			(x1 - x2) + (y2 - y1)
		else
			(x1 - x2) + (y1 - y2)
;;


let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	
	let n = ref 0 in
	let m = ref 0 in
	m := List.length input_list - 1;
	n := (
		match input_list with
		|[] -> 0
		|t::q -> String.length t
	);
	let input_tab = Array.make_matrix !m !n '.' in
	parse input_list input_tab 0;

	let vc = void_columns input_tab !m !n in
	let vl = void_lines input_tab !m !n in

	let expand_tab = Array.make_matrix (!m + vl) (!n + vc) '.' in
	expand input_tab expand_tab !m !n;

	let galaxy_coords = list_to_array (galaxy_coords_list expand_tab (!m + vl) (!n + vc)) in

	let result = ref 0 in
	for i = 0 to (Array.length galaxy_coords) - 1 do
		for j = i to (Array.length galaxy_coords) - 1 do
			result := !result + (distance_manhattan galaxy_coords.(i) galaxy_coords.(j))
		done
	done;
	print_tab expand_tab;
	print_string "\n";
	print_int !result;
	print_string "\n"
