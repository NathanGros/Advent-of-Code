let stringlist_to_intarray l =
	let arr = Array.make (List.length l) 0 in
	let rec aux arr l i =
		match l with
		|[] -> ()
		|t::q -> arr.(i) <- int_of_string t; aux arr q (i+1)
	in
	aux arr l 0;
	arr
;;


let rec parse l tab i =
	match l with
	|[] -> ()
	|[x] -> ()
	|""::s::q -> parse q tab (i+1)
	|t::q when i>=0 ->
		let l2 = String.split_on_char ' ' t in
		let arr = stringlist_to_intarray l2 in
		tab.(i) <- tab.(i) @ [arr];
		parse q tab i
	|t::q -> parse q tab i
;;


let rec parse_seeds tab l i =
	match l with
	|[] -> ()
	|t::q -> tab.(i) <- int_of_string t; parse_seeds tab q (i+1)
;;


let next seeds l =
	let rec find n l =
		match l with
		|[] -> n
		|t::q ->
			if (n >= t.(1)) && (n < t.(1) + t.(2)) then
				n - t.(1) + t.(0)
			else
				find n q
	in
	for i = 0 to Array.length seeds - 1 do
		let seed_val = seeds.(i) in
		seeds.(i) <- find seed_val l
	done
;;


let print_tab tab =
	for i = 0 to Array.length tab - 1 do
		print_int tab.(i);
		print_string " "
	done;
	print_string "\n"
;;


let print_data tab =
	let rec print_arr_list l =
		match l with
		|[] -> print_string "\n"
		|t::q -> print_tab t; print_arr_list q
	in
	for i = 0 to 7 do
		print_arr_list tab.(i)
	done
;;


let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;

	let data_tab = Array.init 8 (fun f -> []) in
	parse input_list data_tab (-1);

	let seeds_list = match input_list with |[] -> [] |t::q -> String.split_on_char ' ' (String.sub t 7 (String.length t - 7)) in
	let seeds = Array.make (List.length seeds_list) 0 in
	parse_seeds seeds seeds_list 0;
	
	for i = 0 to 7 do
		print_tab seeds;
		next seeds data_tab.(i)
	done;

	let result = ref seeds.(0) in
	for i = 0 to Array.length seeds - 1 do
		if seeds.(i) < !result then result := seeds.(i)
	done;
	print_string "\n";
	print_int !result;
	print_string "\n"
