let parse l = (* (string * int list) array *)
	let tab = Array.init (List.length l - 1) (fun i -> Array.of_list (String.split_on_char ' ' (List.nth l i))) in
	let tab2 = Array.init (List.length l - 1) (fun i -> (tab.(i).(0), Array.of_list (String.split_on_char ',' tab.(i).(1))) ) in
	Array.init (List.length l - 1) (fun i -> (tab.(i).(0), List.init (Array.length (snd tab2.(i))) (fun j -> int_of_string (snd tab2.(i)).(j) )))
;;


let print_tab tab =
	let n = Array.length tab in
	for i = 0 to n - 1 do
		let s, a = tab.(i) in
		for j = 0 to String.length s - 1 do
			print_string (String.make 1 (String.get s j))
		done;
		print_string "  ";
		for k = 0 to Array.length a - 1 do
			print_int a.(k);
			print_string " "
		done;
		print_string "\n"
	done
;;


let is_solution input_str str =
	let flag = ref true in
	for i = 0 to String.length input_str - 1 do
		if String.get input_str i <> '?' && String.get input_str i <> String.get str i then
			flag := false
	done;
	!flag
;;



let rec list_sum l=
	match l with
	|[] -> (-1)
	|t::q -> (1+t) + list_sum q
;;


let rec count_patterns (input_str: string) (str: string) (l: int list) (nb_cases: int) =
	match l with
	|[] ->
		let final_str = String.cat str (String.make (nb_cases + 1) '.') in
		if is_solution input_str final_str then begin
			print_string final_str;
			print_string "\n";
			1
		end
		else 0
	|t::q ->
		let sum = list_sum l in
		let res = ref 0 in
		for i = 0 to nb_cases - sum do
			let nb_cases2 = nb_cases - List.hd l - 1 - i in
			let str2 = match str with
				|"" -> String.cat (String.make i '.') (String.make (List.hd l) '#')
				|s -> String.cat s (String.cat (String.make (i+1) '.') (String.make (List.hd l) '#'))
			in
			res := !res + count_patterns input_str str2 (List.tl l) nb_cases2
		done;
		!res
;;


let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let tab = parse input_list in

	let res = ref 0 in
	for i = 0 to Array.length tab - 1 do
		res := !res + count_patterns (fst tab.(i)) "" (snd tab.(i)) (String.length (fst tab.(i)));
		print_string "\n"
	done;
	print_int !res;
	print_string "\n"
