let parse l =
	let n = List.length l - 1 in
	let m = match l with |[] -> 0 |t::q -> String.length t in
	let tab = Array.make_matrix n m ' ' in
	let rec aux l tab i =
		match l with
		|[] -> ()
		|[x] -> ()
		|t::q ->
			for j = 0 to String.length t - 1 do
				tab.(i).(j) <- String.get t j
			done;
			aux q tab (i+1)
	in
	aux l tab 0;
	tab
;;


let tilt_north tab =
	for i = 0 to Array.length tab - 1 do
		for j = 0 to Array.length tab.(0) - 1 do
			if tab.(i).(j) = 'O' then begin
				let k = ref (i - 1) in
				while (!k >= 0) && (tab.(!k).(j) = '.') do
					k := !k - 1
				done;
				k := !k + 1;
				tab.(i).(j) <- '.';
				tab.(!k).(j) <- 'O'
			end
		done
	done
;;


let weight tab =
	let result = ref 0 in
	for i = 0 to Array.length tab - 1 do
		for j = 0 to Array.length tab.(0) - 1 do
			if tab.(i).(j) = 'O' then
				result := !result + (Array.length tab - i)
		done
	done;
	!result
;;


let print_matrix tab =
	for i = 0 to Array.length tab - 1 do
		for j = 0 to Array.length tab.(0) - 1 do
			print_string (String.make 1 tab.(i).(j));
		done;
		print_string "\n"
	done
;;


let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let tab = parse input_list in

	tilt_north tab;
	print_matrix tab;
	
	let result = weight tab in
	print_string "\n";
	print_int result;
	print_string "\n"
