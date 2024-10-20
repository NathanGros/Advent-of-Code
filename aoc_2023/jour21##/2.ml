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

let count mat =
	let result = ref 0 in
	for i = 0 to Array.length mat - 1 do
		for j = 0 to Array.length mat.(0) - 1 do
			if mat.(i).(j) = 'O' then result := !result + 1
		done
	done;
	!result
;;

let print_mat mat =
	Array.iter (fun a -> Array.iter (fun c -> print_string (String.make 1 c)) a; print_string "\n") mat;
	print_int (count mat);
	print_string "\n"
;;

let _ = (*65 + 202300 * 131 = 2650135*)
(*parse*)
	let width = 131 in
	let file = open_in "input.txt" in
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
	let mat_corner1 = Array.map (fun a -> Array.map (fun c -> if c = '#' then c else '.') a) mat in
	let mat_corner2 = Array.map (fun a -> Array.map (fun c -> if c = '#' then c else '.') a) mat in
	let mat_corner3 = Array.map (fun a -> Array.map (fun c -> if c = '#' then c else '.') a) mat in
	let mat_corner4 = Array.map (fun a -> Array.map (fun c -> if c = '#' then c else '.') a) mat in
	let mat_edge1 = Array.map (fun a -> Array.map (fun c -> if c = '#' then c else '.') a) mat in
	let mat_edge2 = Array.map (fun a -> Array.map (fun c -> if c = '#' then c else '.') a) mat in
	let mat_edge3 = Array.map (fun a -> Array.map (fun c -> if c = '#' then c else '.') a) mat in
	let mat_edge4 = Array.map (fun a -> Array.map (fun c -> if c = '#' then c else '.') a) mat in
	let mat_me1 = Array.map (fun a -> Array.map (fun c -> if c = '#' then c else '.') a) mat in
	let mat_me2 = Array.map (fun a -> Array.map (fun c -> if c = '#' then c else '.') a) mat in
	let mat_me3 = Array.map (fun a -> Array.map (fun c -> if c = '#' then c else '.') a) mat in
	let mat_me4 = Array.map (fun a -> Array.map (fun c -> if c = '#' then c else '.') a) mat in
	mat_corner1.(0).(0) <- 'O';
	mat_corner2.(width - 1).(0) <- 'O';
	mat_corner3.(width - 1).(width - 1) <- 'O';
	mat_corner4.(0).(width - 1) <- 'O';
	mat_edge1.(0).(0) <- 'O';
	mat_edge2.(width - 1).(0) <- 'O';
	mat_edge3.(width - 1).(width - 1) <- 'O';
	mat_edge4.(0).(width - 1) <- 'O';
	mat_me1.(width/2).(0) <- 'O';
	mat_me2.(0).(width/2) <- 'O';
	mat_me3.(width/2).(width-1) <- 'O';
	mat_me4.(width-1).(width/2) <- 'O';
	for i = 1 to (width - 5) / 2 do
		one_step mat_corner1;
		one_step mat_corner2;
		one_step mat_corner3;
		one_step mat_corner4
	done;
	for i = 1 to (width - 5) / 2 + width + 1 do
		one_step mat_edge1;
		one_step mat_edge2;
		one_step mat_edge3;
		one_step mat_edge4
	done;
	for i = 1 to width - 1 do
		one_step mat_me1;
		one_step mat_me2;
		one_step mat_me3;
		one_step mat_me4
	done;
	for i = 1 to width do
		one_step mat
	done;

(*count*)
	let nb_me_1 = count mat_me1 in
	let nb_me_2 = count mat_me2 in
	let nb_me_3 = count mat_me3 in
	let nb_me_4 = count mat_me4 in
	let nb_e_1 = count mat_edge1 in
	let nb_e_2 = count mat_edge2 in
	let nb_e_3 = count mat_edge3 in
	let nb_e_4 = count mat_edge4 in
	let nb_m = count mat in
	one_step mat;
	one_step mat_corner1;
	one_step mat_corner2;
	one_step mat_corner3;
	one_step mat_corner4;
	let nb_mp = count mat in
	let nb_c_1p = count mat_corner1 in
	let nb_c_2p = count mat_corner2 in
	let nb_c_3p = count mat_corner3 in
	let nb_c_4p = count mat_corner4 in
	let n = 202300 in
	let result =
		n * n * nb_mp
		+ (n-1) * (n-1) * nb_m
		+ (n-1) * (nb_e_1 + nb_e_2 + nb_e_3 + nb_e_4)
		+ n * (nb_c_1p + nb_c_2p + nb_c_3p + nb_c_4p)
		+ nb_me_1 + nb_me_2 + nb_me_3 + nb_me_4
	in
	print_string "\n Reachable tiles : ";
	print_int result;
	print_string "\n"
;;
