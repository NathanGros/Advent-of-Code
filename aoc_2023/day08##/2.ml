let rec parse l tab n =
	match l with
	|[] -> ()
	|[x] -> ()
	|t::q ->
		if String.contains t '=' then	begin
			let l2 = String.split_on_char '=' t in
			let atemp, btemp =
				match l2 with
				|x::y::q -> x, y
				|_ -> "", ""
			in
			let a = String.sub atemp 0 3 in
			let b = String.sub btemp 2 3 in
			let c = String.sub btemp 7 3 in
			tab.(n).(0) <- a;
			tab.(n).(1) <- b;
			tab.(n).(2) <- c;
		end;
		parse q tab (n+1)
;;


let prime n =
	let flag = ref true in
	for i = 2 to int_of_float (sqrt (float_of_int n)) + 1 do
		if n mod i = 0 then
			flag := false
	done;
	!flag
;;


let rec smallest_factor n i =
	if n mod i = 0 then i else smallest_factor n (i+1)
;;


let rec prime_factors n =
	match prime n with
	|true -> [n]
	|false ->
		let a = smallest_factor n 2 in
		a::(prime_factors (n/a))
;;


let list_to_array l =
	let n = List.length l in
	let t = Array.make n 0 in
	let rec aux l t i =
		match l with
		|[] -> ()
		|x::q -> t.(i) <- x; aux q t (i+1)
	in
	aux l t 0;
	t
;;


let ppcm a b =
	let la = prime_factors a in
	let lb = prime_factors b in
	let ta = list_to_array la in
	let tb = list_to_array lb in
	let t = Array.sort compare (list_to_array (la @ lb)) in
	()
;;


let rec print_list l =
	match l with
	|[] -> print_string "\n"
	|t::q -> print_int t; print_string " "; print_list q
;;


let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let tab = Array.init (List.length input_list - 2) (fun f -> [|""; ""; ""|]) in
	parse input_list tab (-2);
	let input_directions =
		match input_list with
		|a::b -> a
		|_ -> ""
	in
	let path = Array.make (String.length input_directions) 1 in
	for i = 0 to Array.length path - 1 do
		if String.get input_directions i = 'R' then
			path.(i) <- 2
	done;
	let n = Array.length tab in
	let path_number = ref 0 in
	for i = 0 to n - 2 do
		if String.sub tab.(i).(0) 2 1 = "A" then path_number := !path_number + 1
	done;
	let path_lengths = Array.make !path_number 0 in
	let path_index = Array.make !path_number 0 in
	let count = ref 0 in
	for i = 0 to n - 2 do
		if String.sub tab.(i).(0) 2 1 = "A" then begin
			path_index.(!count) <- i;
			count := !count + 1
		end
	done;

	for i = 0 to !path_number - 1 do
		let length = ref 0 in
		let indice = ref path_index.(i) in
		while (not (String.sub tab.(!indice).(0) 2 1 = "Z")) do
			let tempstring = tab.(!indice).(path.(!length mod (Array.length path))) in
			for j = 0 to n - 2 do
				if tab.(j).(0) = tempstring then indice := j
			done;
			length := !length + 1
		done;
		path_lengths.(i) <- !length
	done;
	
	for i = 0 to !path_number - 1 do
		print_int path_lengths.(i);
		print_string "\n";
		print_list (prime_factors path_lengths.(i));
		print_string "\n"
	done;
	print_string "\n";
