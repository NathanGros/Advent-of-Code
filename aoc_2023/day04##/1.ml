let rec int_list l =
	match l with
	|[] -> []
	|t::q -> (int_of_string t) :: int_list q
;;


let rec remove_empty l =
	match l with
	|[] -> []
	|""::q -> remove_empty q
	|x::q -> x::(remove_empty q)
;;


let rec execute_input_list l tab n =
	match l with
	|[] -> tab
	|t::q ->
		let card_string = String.sub t 10 (String.length t - 10) in
		let card_list_temp = String.split_on_char '|' card_string in
		let l1str, l2str =
			match card_list_temp with
			|[a;b] -> String.split_on_char ' ' a, String.split_on_char ' ' b
			|_ -> [], []
		in
		let l1int, l2int = int_list (remove_empty l1str), int_list (remove_empty l2str) in
		tab.(n) <- (l1int, l2int);
		execute_input_list q tab (n+1)
;;


let build_tab l =
	let tab = Array.make (List.length l) ([], []) in
	execute_input_list l tab 0
;;


let rec in_list x l =
	match l with
	|[] -> false
	|t::q ->
		if t = x then
			true
		else
			in_list x q
;;


let rec score l1 l2 n =
	match l2 with
	|[] -> n
	|t::q ->
		if in_list t l1 then
			if n = 0 then
				score l1 q 1
			else
				score l1 q (2*n)
		else
			score l1 q n
;;


let rec remove_last l =
	match l with
	|[] -> []
	|a::b::[] -> a::[]
	|t::q -> t::(remove_last q)
;;


let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list_temp = String.split_on_char '\n' input_string in
	close_in file;
	let input_list = remove_last input_list_temp in
	let tab = build_tab input_list in
	let result = ref 0 in
	for i = 0 to Array.length tab - 1 do
		result := !result + score (fst tab.(i)) (snd tab.(i)) 0
	done;
	print_int !result;
	print_string "\n"
