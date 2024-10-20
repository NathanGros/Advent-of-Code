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


let rec matching_nb l1 l2 n =
	match l2 with
	|[] -> n
	|t::q ->
		if in_list t l1 then
			matching_nb l1 q (n+1)
		else
			matching_nb l1 q n
;;


let rec remove_last l =
	match l with
	|[] -> []
	|a::b::[] -> a::[]
	|t::q -> t::(remove_last q)
;;


let print_set t =
	for i = 0 to Array.length t - 1 do
		for j = 0 to 1 do
			print_int t.(i).(j);
			print_string " ";
		done;
		print_string "\n"
	done
;;

let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list_temp = String.split_on_char '\n' input_string in
	close_in file;
	let input_list = remove_last input_list_temp in
	let tab = build_tab input_list in
	let card_score_tab = Array.make_matrix (Array.length tab) 2 1 in
	for i = 0 to Array.length tab - 1 do
		card_score_tab.(i).(1) <- matching_nb (fst tab.(i)) (snd tab.(i)) 0
	done;
	for i = 0 to Array.length card_score_tab - 1 do
		let n = card_score_tab.(i).(1) in
		let a = card_score_tab.(i).(0) in
		for j = 1 to n do
			card_score_tab.(i+j).(0) <- card_score_tab.(i+j).(0) + a
		done
	done;
	(*print_set card_score_tab*)
	let result = ref 0 in
	for i = 0 to Array.length card_score_tab - 1 do
		result := !result + card_score_tab.(i).(0)
	done;
	print_int !result;
	print_string "\n"
