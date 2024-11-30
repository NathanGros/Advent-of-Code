let hash s =
	let index = ref 0 in
	let i = ref 0 in
	while (String.get s !i <> '=') && (String.get s !i <> '-') do
		let c = String.get s !i in
		let code = Char.code c in
		index := !index + code;
		index := !index * 17;
		index := !index mod 256;
		i := !i + 1;
	done;
	let operation = if String.get s !i = '=' then 1 else 0 in
	let foc_len = if operation = 0 then None else Some (int_of_string (String.make 1 (String.get s (!i+1)))) in
	!index, operation, foc_len, (String.sub s 0 !i)
;;


let rec in_list label l =
	match l with
	|[] -> false
	|(labelx, _)::q -> if labelx = label then true else in_list label q
;;


let replace_lens tab index foc_len label =
	let l = tab.(index) in
	let rec replace l foc_len label =
		match l with
		|[] -> []
		|(labelx, foc_lenx)::q ->
			if labelx = label then
				(label, foc_len)::q
			else
				(labelx, foc_lenx)::(replace q foc_len label)
	in
	let l2 = replace l foc_len label in
	tab.(index) <- l2
;;


let rec append tab index foc_len label =
	let l = tab.(index) in
	let rec append l x =
		match l with
		|[] -> [x]
		|t::q -> t::(append q x)
	in
	let l2 = append l (label, foc_len) in
	tab.(index) <- l2
;;


let remove_lens tab index label =
	let l = tab.(index) in
	let rec remove l label =
		match l with
		|[] -> []
		|(labelx, foc_lenx)::q ->
			if labelx = label then
				q
			else
				(labelx, foc_lenx)::(remove q label)
	in
	let l2 = remove l label in
	tab.(index) <- l2
;;


let apply_string s tab =
	let index, op, foc_len, label = hash s in
	if op = 1 then
		if in_list label tab.(index) then
			replace_lens tab index foc_len label
		else
			append tab index foc_len label
	else
		remove_lens tab index label
;;


let rec execute_input l tab =
	match l with
	|[] -> ()
	|t::q ->
		apply_string t tab;
		execute_input q tab
;;


let rec focus_power l n =
	match l with
	|[] -> 0
	|(_, Some foc_len)::q ->
		n * foc_len + (focus_power q (n+1))
	|_ -> failwith "error"
;;


let rec print_list l =
	match l with
	|[] -> ()
	|(label, Some foc_len)::q ->
		print_string label;
		print_string " ";
		print_int foc_len;
		print_string " ";
		print_list q
	|_ -> failwith "error"
;;

let print_tab tab =
	for i = 0 to Array.length tab - 1 do
		print_list tab.(i);
		print_string "\n"
	done
;;

let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let input_string2 = match input_list with |t::q -> t |_ -> failwith "error" in
	let input_list2 = String.split_on_char ',' input_string2 in

	let tab = Array.init 256 (fun f -> []) in
	execute_input input_list2 tab;

	let result = ref 0 in
	for i = 0 to Array.length tab - 1 do
		result := !result + (i+1) * (focus_power tab.(i) 1)
	done;

	print_int !result;
	print_string "\n";
