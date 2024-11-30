let string_list_to_int_array l =
	let tab = Array.make (List.length l) 0 in
	let rec aux l tab i =
		match l with
		|[] -> ()
		|t::q ->
			tab.(i) <- int_of_string t;
			aux q tab (i+1)
	in
	aux l tab 0;
	tab
;;


let rec parse l tab i =
	match l with
	|[] -> ()
	|[x] -> ()
	|t::q ->
		tab.(i) <- string_list_to_int_array (String.split_on_char ' ' t);
		parse q tab (i+1)
;;


let previous_number t =
	let n = Array.length t in
	let decomp_tab = Array.make_matrix n (n+1) 0 in
	for i = 0 to n-1 do
		decomp_tab.(0).(i) <- t.(i)
	done;
	for i = 1 to n-1 do
		for j = 0 to (n-i-1) do
			decomp_tab.(i).(j) <- decomp_tab.(i-1).(j+1) - decomp_tab.(i-1).(j)
		done
	done;
	for i = n-2 downto 0 do
		decomp_tab.(i).(n-i) <- decomp_tab.(i).(0) - decomp_tab.(i+1).(n-i-1)
	done;
	decomp_tab.(0).(n)
;;


let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	
	let input_tab = Array.init (List.length input_list - 1) (fun f -> [||]) in
	parse input_list input_tab 0;

	let result = ref 0 in
	for i = 0 to Array.length input_tab - 1 do
		result := !result + (previous_number input_tab.(i))
	done;

	print_int !result;
	print_string "\n"
