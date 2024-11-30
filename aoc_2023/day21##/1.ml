let one_step mat =
	let mat2 = Array.map (fun a -> Array.map (fun c -> if c = '#' then c else '.') a) mat in
	for i = 0 to Array.length mat2 - 1 do
		for j = 0 to Array.length mat2.(0) - 1 do
			if mat.(i).(j) <> '#' then begin
				if i > 0 && mat.(i-1).(j) = 'O' then mat2.(i).(j) <- 'O';
				if j > 0 && mat.(i).(j-1) = 'O' then mat2.(i).(j) <- 'O';
				if i < (Array.length mat2 - 1) && mat.(i+1).(j) = 'O' then mat2.(i).(j) <- 'O';
				if j < (Array.length mat2.(0) - 1) && mat.(i).(j+1) = 'O' then mat2.(i).(j) <- 'O'
			end
		done
	done;
	for i = 0 to Array.length mat - 1 do
		for j = 0 to Array.length mat.(0) - 1 do
			if mat.(i).(j) = 'O' then mat.(i).(j) <- '.';
			mat.(i).(j) <- mat2.(i).(j)
		done
	done;
;;

let _ =
(*parse*)
	let file = open_in "test_input2.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let tab_temp = Array.init (List.length input_list - 1) (fun i -> List.nth input_list i) in
	let mat = Array.map (fun s -> Array.init (String.length s) (fun i -> String.get s i)) tab_temp in
	for i = 0 to Array.length mat - 1 do
		for j = 0 to Array.length mat.(0) - 1 do
			if mat.(i).(j) = 'S' then mat.(i).(j) <- 'O'
		done
	done;

(*steps*)
	for i = 1 to 27 do
		one_step mat
	done;
	Array.iter (fun a -> Array.iter (fun c -> print_string (String.make 1 c)) a; print_string "\n") mat;

(*count*)
	let result = ref 0 in
	for i = 0 to Array.length mat - 1 do
		for j = 0 to Array.length mat.(0) - 1 do
			if mat.(i).(j) = 'O' then result := !result + 1
		done
	done;
	print_string "\n Reachable tiles : ";
	print_int !result;
	print_string "\n"
;;
