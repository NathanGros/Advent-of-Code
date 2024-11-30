type direction =
	|N
	|S
	|E
	|W
;;

type node = (int * int);;

type mat_adj = (node * (node * int) list) array;;

let max_list l =
	let rec aux l m =
		match l with
		|[] -> m
		|h::t -> if h>m then aux t h else aux t m
	in
	aux l 0
;;

let is_node mat i j =
	match mat.(i).(j), mat.(i-1).(j), mat.(i).(j-1), mat.(i+1).(j), mat.(i).(j+1) with
	|'#', _, _, _, _ -> false
	|_, c1, c2, c3, c4 ->
		let nb_paths = ref 0 in
		if c1 <> '#' then nb_paths := !nb_paths + 1;
		if c2 <> '#' then nb_paths := !nb_paths + 1;
		if c3 <> '#' then nb_paths := !nb_paths + 1;
		if c4 <> '#' then nb_paths := !nb_paths + 1;
		!nb_paths > 2
;;

let get_nodes mat =
	let h, w = Array.length mat, Array.length mat.(0) in
	let l = ref [] in
	for i = 1 to h - 2 do
		for j = 1 to w - 2 do
			if is_node mat i j then
				l := (i, j) :: (!l)
		done
	done;
	!l
;;

let get_exits mat =
	let h, w = Array.length mat, Array.length mat.(0) in
	let l = ref [] in
	for i = 0 to h - 1 do
			if mat.(i).(0) = '.' then l := (i, 0) :: (!l)
	done;
	for i = 0 to h - 1 do
			if mat.(i).(w - 1) = '.' then l := (i, w - 1) :: (!l)
	done;
	for j = 0 to w - 1 do
			if mat.(0).(j) = '.' then l := (0, j) :: (!l)
	done;
	for j = 0 to w - 1 do
			if mat.(h - 1).(j) = '.' then l := (h - 1, j) :: (!l)
	done;
	!l
;;

let rec follow_path mat i j dir n =
	if mat.(i).(j) = 'N' || mat.(i).(j) = 'E' then (i, j), n
	else
		match dir with
		|N ->
			if mat.(i-1).(j) <> '#' then
				follow_path mat (i-1) j N (n+1)
			else
				if mat.(i).(j-1) <> '#' then
					follow_path mat i (j-1) W (n+1)
				else
					follow_path mat i (j+1) E (n+1)
		|S ->
			if mat.(i+1).(j) <> '#' then
				follow_path mat (i+1) j S (n+1)
			else
				if mat.(i).(j-1) <> '#' then
					follow_path mat i (j-1) W (n+1)
				else
					follow_path mat i (j+1) E (n+1)
		|E ->
			if mat.(i).(j+1) <> '#' then
				follow_path mat i (j+1) E (n+1)
			else
				if mat.(i-1).(j) <> '#' then
					follow_path mat (i-1) j N (n+1)
				else
					follow_path mat (i+1) j S (n+1)
		|W ->
			if mat.(i).(j-1) <> '#' then
				follow_path mat i (j-1) W (n+1)
			else
				if mat.(i-1).(j) <> '#' then
					follow_path mat (i-1) j N (n+1)
				else
					follow_path mat (i+1) j S (n+1)
;;

let build_adj_list mat x y =
	let l = ref [] in
	if mat.(x-1).(y) = ' ' then l := (follow_path mat (x-2) y N 2) :: (!l);
	if mat.(x).(y-1) = ' ' then l := (follow_path mat x (y-2) W 2) :: (!l);
	if mat.(x+1).(y) = ' ' then l := (follow_path mat (x+2) y S 2) :: (!l);
	if mat.(x).(y+1) = ' ' then l := (follow_path mat x (y+2) E 2) :: (!l);
	!l
;;

let build_mat_adj mat nodes_tab exits_tab =
	Array.init (Array.length nodes_tab + Array.length exits_tab) (
	fun i ->
		if i > 1 then
			let node_x, node_y = nodes_tab.(i-2) in
			(node_x, node_y), (build_adj_list mat node_x node_y)
		else if i = 1 then
			let start_x, start_y = exits_tab.(1) in
			(start_x, start_y), [follow_path mat (start_x + 1) start_y S 1]
		else
			let end_x, end_y = exits_tab.(0) in
			(end_x, end_y), []
	)
;;

let rec print_adj_list l =
	match l with
	|[] -> print_string "\n"
	|((node_x, node_y), n)::t ->
		print_string "(";
		print_int node_x;
		print_string ", ";
		print_int node_y;
		print_string ") : ";
		print_int n;
		print_string " | ";
		print_adj_list t
;;

let print_adj mat_adj =
	Array.iter (
	fun ((nx, ny), l) ->
		print_string "(";
		print_int nx;
		print_string ", ";
		print_int ny;
		print_string ") -> ";
		print_adj_list l
	) mat_adj
;;

let rec print_node_list l =
	match l with
	|[] -> print_string "\n"
	|(x, y)::t -> print_int x; print_string ","; print_int y; print_string ";"; print_node_list t
;;

let rec longest tab (x, y) used =
	if List.hd used = (123, 131) then 125
	else begin
		let temp_l = ref [] in
		for i = 0 to Array.length tab - 1 do
			if fst tab.(i) = (x, y) then temp_l := snd tab.(i)
		done;
		let l = !temp_l in
		if l = [] then 0
		else
			max_list (
				List.map (fun ((nx, ny), n) ->
					if List.mem (nx, ny) used then -10000000
					else
						n + longest tab (nx, ny) ((nx, ny)::used)
				) l
			)
	end
;;

let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let mat_temp = Array.init (List.length input_list - 1) (fun i -> List.nth input_list i) in
	let mat = Array.map (fun s -> Array.init (String.length s) (fun i -> String.get s i)) mat_temp in

	Array.iter (fun a -> Array.iter (fun c -> print_string (String.make 1 c); print_string " ") a; print_string "\n") mat;

	let exits_tab = Array.of_list (get_exits mat) in
	let nodes_tab = Array.of_list (get_nodes mat) in
	for i = 0 to Array.length mat - 1 do
		for j = 0 to Array.length mat.(0) - 1 do
			if mat.(i).(j) = '.' then mat.(i).(j) <- ' ';
			if mat.(i).(j) = '<' then mat.(i).(j) <- ' ';
			if mat.(i).(j) = '>' then mat.(i).(j) <- ' ';
			if mat.(i).(j) = '^' then mat.(i).(j) <- ' ';
			if mat.(i).(j) = 'v' then mat.(i).(j) <- ' '
		done
	done;
	Array.iter (fun (node_x, node_y) -> mat.(node_x).(node_y) <- 'N') nodes_tab;
	Array.iter (fun (exit_x, exit_y) -> mat.(exit_x).(exit_y) <- 'E') exits_tab;
	print_string "\n";
	Array.iter (fun a -> Array.iter (fun c -> print_string (String.make 1 c); print_string " ") a; print_string "\n") mat;

	let mat_adj = build_mat_adj mat nodes_tab exits_tab in
	
	print_string "\n";
	print_adj mat_adj;
	
	let result = longest mat_adj (0, 1) [(0, 1)] in
	print_string "\nThe longest path is ";
	print_int result;
	print_string " long\n"
;;
