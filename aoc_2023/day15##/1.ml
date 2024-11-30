let hash s =
	let n = String.length s in
	let result = ref 0 in
	for i = 0 to n - 1 do
		let c = String.get s i in
		let code = Char.code c in
		result := !result + code;
		result := !result * 17;
		result := !result mod 256
	done;
	!result
;;


let rec execute l res =
	match l with
	|[] -> res
	|t::q -> (hash t) + (execute q res)
;;


let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let input_string2 = match input_list with |t::q -> t |_ -> failwith "error" in
	let input_list2 = String.split_on_char ',' input_string2 in
	let result = execute input_list2 0 in
	print_int result;
	print_string "\n"
