let rec pow a n =
	match n with
	|0 -> 1
	|m -> pow a (n-1) * a
;;

let int_of_hex s =
	let res = ref 0 in
	for i = 0 to String.length s - 1 do
		let digit_hex = String.get s (String.length s - 1 - i) in
		let digit_dec = match digit_hex with
			|'0' -> 0
			|'1' -> 1
			|'2' -> 2
			|'3' -> 3
			|'4' -> 4
			|'5' -> 5
			|'6' -> 6
			|'7' -> 7
			|'8' -> 8
			|'9' -> 9
			|'a' -> 10
			|'b' -> 11
			|'c' -> 12
			|'d' -> 13
			|'e' -> 14
			|'f' -> 15
			|_ -> failwith "int of hex error"
		in
		res := !res + (pow 16 i) * digit_dec
	done;
	!res
;;

let rec remove_duplicates l =
	match l with
	|[] -> []
	|h::t -> if List.mem h t then remove_duplicates t else h::remove_duplicates t
;;

let rec build_grid_lists tab w_list h_list i =
	match i with
	|0 ->
		List.sort compare (remove_duplicates w_list),
		List.sort compare (remove_duplicates h_list)
	|n ->	let w, h = tab.(i) in build_grid_lists tab (w::(w+1)::w_list) (h::(h+1)::h_list) (i-1)
;;

let rec downscale n l i =
	match l with
	|[] -> failwith "grid error"
	|h::t -> if h = n then i else downscale n t (i+1)
;;

let upscale n l =
	List.nth l n
;;

let rec flood tab w h =
	tab.(w).(h) <- '#';
	if tab.(w-1).(h) <> '#' then flood tab (w-1) h;
	if tab.(w+1).(h) <> '#' then flood tab (w+1) h;
	if tab.(w).(h-1) <> '#' then flood tab w (h-1);
	if tab.(w).(h+1) <> '#' then flood tab w (h+1)
;;

let fill tab =
	let grid = Array.map (fun a -> Array.map (fun c -> c) a) tab in
	let printing = ref false in
	(*print horizontal outline*)
	for i = 0 to Array.length grid - 1 do
		for j = 0 to Array.length grid.(0) - 1 do
			if tab.(i).(j) = '#' then printing := not !printing;
			if !printing then grid.(i).(j) <- '#'
		done
	done;
	(*print vertical outline*)
	for j = 0 to Array.length grid.(0) - 1 do
		for i = 0 to Array.length grid - 1 do
			if tab.(i).(j) = '#' then printing := not !printing;
			if !printing then grid.(i).(j) <- '#'
		done
	done;
	(*flood*)
	let i = ref 0 in
	while grid.(!i).(0) <> '#' do
		i := !i + 1
	done;
	let first_h = !i in
	flood grid (first_h + 1) 1;

	(*copy in tab*)
	for i = 0 to Array.length tab - 1 do
		for j = 0 to Array.length tab.(0) - 1 do
			tab.(i).(j) <- grid.(i).(j)
		done
	done
;;

let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;

	let input_tab_temp = Array.init (List.length input_list - 1) (fun i -> List.nth (String.split_on_char '#' (List.nth input_list i)) 1) in
	let input_tab = Array.map (fun s -> int_of_hex (String.sub s 0 5), int_of_string (String.sub s 5 1)) input_tab_temp in

(*remove negatives*)
	let lowest_w = ref 0 in
	let lowest_h = ref 0 in
	let largest_w = ref 0 in
	let largest_h = ref 0 in
	let current_w = ref 0 in
	let current_h = ref 0 in

	for i = 0 to Array.length input_tab - 1 do
		let pos, dir = input_tab.(i) in
		if dir = 0 then current_w := !current_w + pos
		else if dir = 1 then current_h := !current_h + pos
		else if dir = 2 then current_w := !current_w - pos
		else current_h := !current_h - pos
		;
		if !current_w < !lowest_w then lowest_w := !current_w;
		if !current_w > !largest_w then largest_w := !current_w;
		if !current_h < !lowest_h then lowest_h := !current_h;
		if !current_h > !largest_h then largest_h := !current_h
	done;
	let start_w = !lowest_w in
	let start_h = !lowest_h in
	
(*convert directions to points*)
	current_w := (-1) * start_w;
	current_h := (-1) * start_h;
	let points_tab = Array.init (Array.length input_tab) (fun i ->
		let res = (!current_w, !current_h) in
		let pos, dir = input_tab.(i) in
		if dir = 0 then current_w := !current_w + pos
		else if dir = 1 then current_h := !current_h + pos
		else if dir = 2 then current_w := !current_w - pos
		else current_h := !current_h - pos
		;
		res
	) in
	
	print_string "points :\n";
	Array.iter (fun (w, h) -> print_int w; print_string " "; print_int h; print_string "\n") points_tab;
	let w_list, h_list = build_grid_lists points_tab [] [] (Array.length points_tab - 1) in
	print_string "\n";

(*make downscaled pattern*)
	let width = List.length w_list - 1 in
	let height = List.length h_list - 1 in
	let grid = Array.make_matrix width height '.' in
	Array.iter (fun (p_w, p_h) -> grid.(downscale p_w w_list 0).(downscale p_h h_list 0) <- '#') points_tab;

(*fill*)
	fill grid;
	Array.iter (fun a -> Array.iter (fun c -> print_string (String.make 1 c)) a; print_string "\n") grid;

(*calculate area*)
	let result = ref 0 in
	for i = 0 to Array.length grid - 1 do
		for j = 0 to Array.length grid.(0) - 1 do
			if grid.(i).(j) = '#' then
				result := !result + ((upscale (i+1) w_list - upscale i w_list) * (upscale (j+1) h_list - upscale j h_list))
		done
	done;
	print_string "\nThe total area is : ";
	print_int !result;
	print_string "\n"
;;
