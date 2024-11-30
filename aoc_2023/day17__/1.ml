type path =
	|Val of int
	|N of path
	|S of path
	|E of path
	|W of path
;;

let rec print_path (p: path) =
	match p with
	|Val(n) -> print_string " "; print_int n; print_string "\n"
	|N(p2) -> print_string "N"; print_path p2
	|S(p2) -> print_string "S"; print_path p2
	|E(p2) -> print_string "E"; print_path p2
	|W(p2) -> print_string "W"; print_path p2
;;

let parse a =
	Array.map (fun s -> Array.init (String.length s) (fun i -> String.make 1 (String.get s i))) a
;;

let rec remove_last l =
	match l with
	|[] -> []
	|h::[] -> []
	|h::t -> h::remove_last t
;;

let rec fill_mat_path mat p i j =
	match p with
	|N(p2) -> mat.(i-1).(j) <- "."; fill_mat_path mat p2 (i-1) j
	|S(p2) -> mat.(i+1).(j) <- "."; fill_mat_path mat p2 (i+1) j
	|E(p2) -> mat.(i).(j+1) <- "."; fill_mat_path mat p2 i (j+1)
	|W(p2) -> mat.(i).(j-1) <- "."; fill_mat_path mat p2 i (j-1)
	|_ -> ()
;;

let draw_path mat p =
	let path_mat = Array.make_matrix (Array.length mat) (Array.length mat.(0)) " " in
	fill_mat_path path_mat p 0 0;
	path_mat.(0).(0) <- ".";
	for i = 0 to Array.length mat - 1 do
		for j = 0 to Array.length path_mat.(0) - 1 do
			if path_mat.(i).(j) = " " then path_mat.(i).(j) <- mat.(i).(j)
		done
	done;
	Array.iter (fun a -> Array.iter (fun s -> print_string s) a; print_string "\n") path_mat
;;

let rec find_coords p mat x y =
	match p with
	|Val(n) -> x, y
	|N(p2) -> find_coords p2 mat (x-1) y
	|S(p2) -> find_coords p2 mat (x+1) y
	|E(p2) -> find_coords p2 mat x (y+1)
	|W(p2) -> find_coords p2 mat x (y-1)
;;

let rec add_to_path p tail =
	match p with
	|Val(n1) -> (
		match tail with
		|N(Val(n2)) -> N(Val(n1 + n2))
		|S(Val(n2)) -> S(Val(n1 + n2))
		|E(Val(n2)) -> E(Val(n1 + n2))
		|W(Val(n2)) -> W(Val(n1 + n2))
		|_ -> failwith "wtf?"
		)
	|N(p2) -> N(add_to_path p2 tail)
	|S(p2) -> S(add_to_path p2 tail)
	|E(p2) -> E(add_to_path p2 tail)
	|W(p2) -> W(add_to_path p2 tail)
;;

let rec get_heatloss p =
	match p with
	|Val(n) -> n
	|N(p2) |S(p2) |E(p2) |W(p2) -> get_heatloss p2
;;

let rec count_directions p n s e w =
	match p with
	|N(p2) -> count_directions p2 (n+1) s e w
	|S(p2) -> count_directions p2 n (s+1) e w
	|E(p2) -> count_directions p2 n s (e+1) w
	|W(p2) -> count_directions p2 n s e (w+1)
	|_ -> n, s, e, w
;;

let rec is_in_path p x y i j =
	if x=i && y=j then true
	else
		match p with
		|Val(_) -> false
		|N(p2) -> is_in_path p2 x y (i-1) j
		|S(p2) -> is_in_path p2 x y (i+1) j
		|E(p2) -> is_in_path p2 x y i (j+1)
		|W(p2) -> is_in_path p2 x y i (j-1)
;;

let rec remove_four_row l c =
	l
;;

let next_paths p mat =
	let x, y = find_coords p mat 0 0 in
	let h, w = Array.length mat, Array.length mat.(0) in
	let l = ref [] in
	if x > 0 && not (is_in_path p (x-1) y 0 0) then l := !l @ [add_to_path p (N(Val(mat.(x-1).(y))))];
	if y > 0 && not (is_in_path p x (y-1) 0 0) then l := !l @ [add_to_path p (W(Val(mat.(x).(y-1))))];
	if x < h - 1 && not (is_in_path p (x+1) y 0 0) then l := !l @ [add_to_path p (S(Val(mat.(x+1).(y))))];
	if y < w - 1 && not (is_in_path p x (y+1) 0 0) then l := !l @ [add_to_path p (E(Val(mat.(x).(y+1))))];
	remove_four_row !l 0
;;

let rec iter_path_list l mat =
	match l with
	|[] -> []
	|h::t -> next_paths h mat @ iter_path_list t mat
;;

let rec iterate_n l mat n =
	match n with
	|0 -> l
	|x -> iterate_n (iter_path_list l mat) mat (x-1)
;;

let dijkstra_variant mat =
	let res = Array.of_list (iterate_n [Val(mat.(0).(0))] mat 5) in
	for i = 0 to Array.length res - 1 do
		print_path res.(i)
	done;
	print_string "Path number : ";
	print_int (Array.length res);
	print_string "\n\n";
	res
;;

let get_best_path a =
	let current_best_path = ref (Val(-1)) in
	let current_best_heatloss = ref (-1) in
	Array.iter (fun p ->
		let hl = get_heatloss p in
		if !current_best_heatloss > hl || !current_best_path = Val(-1) then begin
			current_best_path := p;
			current_best_heatloss := hl
		end
	) a;
	!current_best_path
;;

let _ =
	(*parsing*)
	let file = open_in "test_input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = remove_last (String.split_on_char '\n' input_string) in
	close_in file;
	let mat = parse (Array.of_list input_list) in
	let mat_int = Array.map (fun a -> Array.map (fun s -> int_of_string s) a) mat in

	let best_path = get_best_path (dijkstra_variant mat_int) in

	draw_path mat best_path;
	print_string "\nThe smallest heat loss is : ";
	print_int (get_heatloss best_path);
	print_string "\n"
;;
