let parse l =
	let max_x = ref 0 in
	let min_x = ref 0 in
	let max_y = ref 0 in
	let min_y = ref 0 in
	let x = ref 1 in
	let y = ref 1 in
	let rec count_max l max_x max_y min_x min_y x y =
		match l with
		|[] -> ()
		|[t] -> ()
		|t::q ->
			let dir = String.get t 0 in
			let nb = ref 0 in
			nb := int_of_string (String.make 1 (String.get t 2));
			if String.get t 3 <> ' ' then
				nb := 10 * !nb + int_of_string (String.make 1 (String.get t 3));
			x := (match dir with
			|'U' -> !x - !nb
			|'D' -> !x + !nb
			|_ -> !x)
			;
			if !x > !max_x then max_x := !x;
			if !x < !min_x then min_x := !x;
			y := (match dir with
			|'L' -> !y - !nb
			|'R' -> !y + !nb
			|_ -> !y)
			;
			if !y > !max_y then max_y := !y;
			if !y < !min_y then min_y := !y;
			count_max q max_x max_y min_x min_y x y
	in
	count_max l max_x max_y min_x min_y x y;
	let tab = Array.make_matrix (1 + !max_x - !min_x) (1 + !max_y - !min_y) '.' in
	tab, (1 + !min_x * (-1)), (1 + !min_y * (-1));
;;


let print_matrix tab =
	for i = 0 to Array.length tab - 1 do
		for j = 0 to Array.length tab.(0) - 1 do
			print_string (String.make 1 tab.(i).(j));
		done;
		print_string "\n"
	done
;;


let rec dig_path tab l x y =
	match l with
	|[] -> ()
	|[t] -> ()
	|t::q ->
		let dir = String.get t 0 in
		let nb = ref 0 in
		nb := int_of_string (String.make 1 (String.get t 2));
		if String.get t 3 <> ' ' then
			nb := 10 * !nb + int_of_string (String.make 1 (String.get t 3));
		let x_dir, y_dir =
			match dir with
			|'U' -> (-1), 0
			|'D' -> 1, 0
			|'L' -> 0, (-1)
			|'R' -> 0, 1
			|_ -> failwith "error 404 dir not found"
		in
		for i = 0 to !nb - 1 do
			x := !x + x_dir;
			y := !y + y_dir;
			tab.(!x).(!y) <- '#'
		done;
		dig_path tab q x y
;;


let rec flow tab x y c =
	if x > 0 && x < (Array.length tab - 1) && y > 0 && y < (Array.length tab.(0) - 1) then
		if not (tab.(x).(y) = '#') then begin
			tab.(x).(y) <- c;
			flow tab x (y-1) c;
			flow tab x (y+1) c;
			flow tab (x-1) y c;
			flow tab (x+1) y c
		end
;;


let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let tab, start_x, start_y = parse input_list in
	dig_path tab input_list (ref start_x) (ref start_y);
	flow tab 80 80 '#';

	print_matrix tab;

	let result = ref 0 in
	for i = 0 to Array.length tab - 1 do
		for j = 0 to Array.length tab.(0) - 1 do
			if tab.(i).(j) = '#' then
				result := !result + 1
		done
	done;

	print_string "\n";
	print_int !result;
	print_string "\n"
