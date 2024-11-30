let int_of_char c =
	match c with
	|'0' |'1' |'2' |'3' |'4' |'5' |'6' |'7' |'8' |'9' -> int_of_string (String.make 1 c)
	|'.' -> -1
	|'*' -> -3
	|_ -> -2
;;


let rec make_input_matrix mat l n =
	match l with
	|[] -> ()
	|str::q ->
		for i = 0 to String.length str - 1 do
			mat.(n).(i) <- int_of_char (String.get str i)
		done;
		make_input_matrix mat q (n+1)
;;


let touch_gear i k mat =
	let flag = ref false in
	let x = ref 0 in
	let y = ref 0 in
	if i>0 then if mat.(i-1).(k) = (-3) then (flag := true; x := i-1; y := k);
	if i<139 then if mat.(i+1).(k) = (-3) then (flag := true; x := i+1; y := k);
	if k>0 then if mat.(i).(k-1) = (-3) then (flag := true; x := i; y := k-1);
	if k<139 then if mat.(i).(k+1) = (-3) then (flag := true; x := i; y := k+1);
	if i>0 && k>0 then if mat.(i-1).(k-1) = (-3) then (flag := true; x := i-1; y := k-1);
	if i>0 && k<139 then if mat.(i-1).(k+1) = (-3) then (flag := true; x := i-1; y := k+1);
	if i<139 && k>0 then if mat.(i+1).(k-1) = (-3) then (flag := true; x := i+1; y := k-1);
	if i<139 && k<139 then if mat.(i+1).(k+1) = (-3) then (flag := true; x := i+1; y := k+1);
	if !flag then
		!x, !y
	else
		(-1), (-1)
;;


let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let input_matrix = Array.make_matrix 140 140 (-1) in
	make_input_matrix input_matrix input_list 0;
	let gear_ratio_matrix = Array.make_matrix 140 140 (false, (-1)) in

	for i = 0 to 139 do
		for j = 0 to 139 do
			if not ((input_matrix.(i).(j) = (-1)) || (input_matrix.(i).(j) = (-2)) || (input_matrix.(i).(j) = (-3))) then
				if (j = 0) || ((input_matrix.(i).(j-1) = (-1)) || (input_matrix.(i).(j-1) = (-2)) || (input_matrix.(i).(j-1) = (-3))) then begin (*debut du nombre*)
					let k = ref j in
					let read_number = ref 0 in
					let flag = ref false in
					let x = ref 0 in
					let y = ref 0 in
					while !k < 140 && not (input_matrix.(i).(!k) = (-1) || input_matrix.(i).(!k) = (-2) || input_matrix.(i).(!k) = (-3)) do
						read_number := !read_number * 10 + input_matrix.(i).(!k);
						let a, b = touch_gear i !k input_matrix in
						if not ((a, b) = ((-1), (-1))) then (
							flag := true;
							x := a;
							y := b
						);
						k := !k + 1
					done;
					if !flag then
						if gear_ratio_matrix.(!x).(!y) = (false, (-1)) then
							gear_ratio_matrix.(!x).(!y) <- (false, !read_number)
						else
							gear_ratio_matrix.(!x).(!y) <- (true, (snd (gear_ratio_matrix.(!x).(!y)) * !read_number))
				end
		done
	done;

	let result = ref 0 in

	for i = 0 to 139 do
		for j = 0 to 139 do
			match gear_ratio_matrix.(i).(j) with
			|true, x -> result := !result + x
			|_ -> ()
		done
	done;
	print_int !result;
	print_string "\n"
