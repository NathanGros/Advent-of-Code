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
	let path = Array.init (String.length input_directions) (fun f -> 1) in
	for i = 0 to Array.length path - 1 do
		if String.get input_directions i = 'R' then
			path.(i) <- 2
	done;
	let n = Array.length tab in
	let result = ref 0 in
	let indice = ref 0 in
	for i = 0 to n - 1 do
		if tab.(i).(0) = "AAA" then indice := i
	done;
	while (not (tab.(!indice).(0) = "ZZZ"))do
		let tempstring = tab.(!indice).(path.(!result mod (Array.length path))) in
		for i = 0 to n - 1 do
			if tab.(i).(0) = tempstring then indice := i
		done;
		result := !result + 1
	done;
	print_int !result;
	print_string "\n"
