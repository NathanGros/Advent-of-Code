let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	close_in file;
	
	let input_list = String.split_on_char '\n' input_string in
	let tab = Array.make_matrix 100 3 0 in

	let rec make_color_tab l line_nb=
		match l with
		|[] -> ()
		|t::q ->
			let n = String.length t in
			for i = 0 to n - 1 do
				if String.starts_with "green" (String.sub t i (n-i)) then
					let a = match (String.get t (i-3)) with
					|'1' -> 1 |'2' -> 2 |'3' -> 3 |'4' -> 4 |'5' -> 5 |'6' -> 6 |'7' -> 7 |'8' -> 8 |'9' -> 9 |_ -> 0
					in
					let b = match (String.get t (i-2)) with
					|'1' -> 1 |'2' -> 2 |'3' -> 3 |'4' -> 4 |'5' -> 5 |'6' -> 6 |'7' -> 7 |'8' -> 8 |'9' -> 9 |_ -> 0
					in
					if (10*a + b) > tab.(line_nb).(0) then tab.(line_nb).(0) <- (10*a + b);
				else 
				if String.starts_with "red" (String.sub t i (n-i)) then
					let a = match (String.get t (i-3)) with
					|'1' -> 1 |'2' -> 2 |'3' -> 3 |'4' -> 4 |'5' -> 5 |'6' -> 6 |'7' -> 7 |'8' -> 8 |'9' -> 9 |_ -> 0
					in
					let b = match (String.get t (i-2)) with
					|'1' -> 1 |'2' -> 2 |'3' -> 3 |'4' -> 4 |'5' -> 5 |'6' -> 6 |'7' -> 7 |'8' -> 8 |'9' -> 9 |_ -> 0
					in
					if (10*a + b) > tab.(line_nb).(1) then tab.(line_nb).(1) <- (10*a + b);
				else
				if String.starts_with "blue" (String.sub t i (n-i)) then
					let a = match (String.get t (i-3)) with
					|'1' -> 1 |'2' -> 2 |'3' -> 3 |'4' -> 4 |'5' -> 5 |'6' -> 6 |'7' -> 7 |'8' -> 8 |'9' -> 9 |_ -> 0
					in
					let b = match (String.get t (i-2)) with
					|'1' -> 1 |'2' -> 2 |'3' -> 3 |'4' -> 4 |'5' -> 5 |'6' -> 6 |'7' -> 7 |'8' -> 8 |'9' -> 9 |_ -> 0
					in
					if (10*a + b) > tab.(line_nb).(2) then tab.(line_nb).(2) <- (10*a + b)
			done;
			make_color_tab q (line_nb + 1)
	in

	make_color_tab input_list 0;

	let result = ref 0 in
	for i = 0 to 99 do
		let x = tab.(i).(0) * tab.(i).(1) * tab.(i).(2) in
		result := !result + x
	done;
	print_int !result;
	print_string "\n"
