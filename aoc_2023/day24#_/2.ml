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

let f a1 b2 a2 b1 =
	a1 *. b2 -. a2 *. b1
;;

let compute_trajectory a1 a2 a3 b1 b2 b3 a1p a2p a3p b1p b2p b3p =
	let ap = (
		f a3 b1 a1 b3 *. f a2p b1p a1p b2p *. a2 *. a1p *. a3p
		+. f a2p b1p a1p b2p *. a2
		+. f b3p a1p a3p b1p *. f a2 b1 a1 b2 *. a3 *. a1p *. a2p
	) /. (
		f a3 b1 a1 b3 *. f a2p b1p a1p b2p *. a2 *. a1p
		+. f a3p b1p a1p b3p *. f a1 b2 a2 b1 *. a1p *. a3
		+. f a3p b1p a1p b3p *. f a2p b1p a1p b2p *. a1 *. a3
	) in
	let a = (ap *. a1 *. a2 *. f a2p b1p a1p b2p) /.
		(a1 *. a1p *. (b2 *. ap -. b2 *. a2p -. b2p *. ap) +. a1p *. a2 *. b1 *. (a2p -. ap) +. a1 *. a2p *. b1p *. ap) in
	a, ap
;;

let _ =
(*parse input*)
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let temp_tab = Array.map (fun s -> String.split_on_char '@' s) (Array.init 3 (fun i -> List.nth input_list i)) in
	let temp2_tab = Array.map (fun l -> let p, v = match l with |[p_l; v_l] -> p_l, v_l |_ -> failwith "parse error" in String.split_on_char ',' p, String.split_on_char ',' v) temp_tab in
	(*get x y z coordinates strings*)
	let temp3_tab = Array.map (fun (p_l, v_l) -> let px, py, pz = match p_l with |[p1; p2; p3] -> p1, p2, p3 |_ -> failwith "parse error" in let vx, vy, vz = match v_l with |[v1; v2; v3] -> v1, v2, v3 |_ -> failwith "parse error" in px, py, pz, vx, vy, vz) temp2_tab in
	(*convert x y z coordinates strings to ints*)
	let pos_tab = Array.map (fun (px, py, pz, vx, vy, vz) -> int_of_string px, int_of_string (del_spaces py), int_of_string (del_spaces pz), int_of_string (del_spaces vx), int_of_string (del_spaces vy), int_of_string (del_spaces vz)) temp3_tab in

(*makes equations tab*)
	let equations_tab_x = Array.map (fun (px, _, pz, vx, _, vz) -> (foi vz /. foi vx), (foi pz -. foi px *. foi vz /. foi vx)) pos_tab in
	let equations_tab_y = Array.map (fun (_, py, pz, _, vy, vz) -> (foi vz /. foi vy), (foi pz -. foi py *. foi vz /. foi vy)) pos_tab in
	
(*printing*)
	Array.iter (fun (a, b) -> print_string "x |--> "; print_float a; print_string " * x + "; print_float b; print_string "\n") equations_tab_x;
	Array.iter (fun (a, b) -> print_string "y |--> "; print_float a; print_string " * y + "; print_float b; print_string "\n") equations_tab_y;

	print_string "\na: ";
	let a, ap = compute_trajectory 1. (-2.) 3. 2. 1. (-5.) 4. 1. (-5.) (-2.) 3. 0. in
	print_float a;
	print_string " , a prime: ";
	print_float ap;
	print_string "\n"
;;
