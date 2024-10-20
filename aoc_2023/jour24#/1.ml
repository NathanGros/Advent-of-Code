let foi = float_of_int;;
let iof = int_of_float;;

let del_spaces s =
	let nb = ref 0 in
	for i = 0 to String.length s - 1 do
		if String.get s i = ' ' then nb := !nb + 1
	done;
	if !nb = 0 then s
	else if !nb = 1 then
		String.sub s 1 (String.length s - 1)
	else
		String.sub s 1 (String.length s - 2)
;;

let is_collision_happening (a1, b1) (a2, b2) boundary_min boundary_max (px1, _, vx1, _) (px2, _, vx2, _)=
	if a1 = a2 then false
	else
		let x = (b2 -. b1) /. (a1 -. a2) in
		let y = a1 *. x +. b1 in
		let in_boundaries = (boundary_min <= x && x <= boundary_max) && (boundary_min <= y && y <= boundary_max) in
		let is_in_past_1 = not ((x < foi px1 && vx1 < 0) || (x > foi px1 && vx1 > 0)) in
		let is_in_past_2 = not ((x < foi px2 && vx2 < 0) || (x > foi px2 && vx2 > 0)) in
		if in_boundaries && (not is_in_past_1 && not is_in_past_2) then begin
			print_float x; print_string ", "; print_float y; print_string "\n"; true
		end else
			false
;;

let _ =
	let boundary_min = 200000000000000. in
	let boundary_max = 400000000000000. in
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let temp_tab = Array.map (fun s -> String.split_on_char '@' s) (Array.init (List.length input_list -1) (fun i -> List.nth input_list i)) in
	let temp2_tab = Array.map (fun l -> let p, v = match l with |[p_l; v_l] -> p_l, v_l |_ -> failwith "parse error" in String.split_on_char ',' p, String.split_on_char ',' v) temp_tab in
	(*get x y coordinates strings*)
	let temp3_tab = Array.map (fun (p_l, v_l) -> let px, py = match p_l with |[p1; p2; _] -> p1, p2 |_ -> failwith "parse error" in let vx, vy = match v_l with |[v1; v2; _] -> v1, v2 |_ -> failwith "parse error" in px, py, vx, vy) temp2_tab in
	(*convert x y coordinates strings to ints*)
	let pos_tab = Array.map (fun (px, py, vx, vy) -> int_of_string px, int_of_string (del_spaces py), int_of_string (del_spaces vx), int_of_string (del_spaces vy)) temp3_tab in
	(*makes equations tab*)
	let equations_tab = Array.map (fun (px, py, vx, vy) -> (foi vy /. foi vx), (foi py -. foi px *. foi vy /. foi vx)) pos_tab in
	
	(*print equations*)
	Array.iter (fun (a, b) -> print_string "x |--> "; print_float a; print_string " * x + "; print_float b; print_string "\n") equations_tab;

	(*check for collision*)
	let result = ref 0 in
	let len = Array.length equations_tab in
	for i = 0 to len - 2 do
		for j = (i+1) to len - 1 do
			print_int i; print_string ", "; print_int j; print_string "\n";
			if (is_collision_happening equations_tab.(i) equations_tab.(j) boundary_min boundary_max pos_tab.(i) pos_tab.(j)) then result := !result + 1
		done
	done;
	print_string "\nThere are ";
	print_int !result;
	print_string " collisions inside the boundaries\n"
;;
