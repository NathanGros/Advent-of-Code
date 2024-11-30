let iof = int_of_string;;

let place_lowest x y axis len highest_mat blocks_mat =
	match axis with
	|'z' -> let h = highest_mat.(x).(y) in blocks_mat.(x).(y).(h) <- axis, len; highest_mat.(x).(y) <- h + len
	|'y' ->
		let h = ref 0 in
		for i = 0 to len - 1 do
			if highest_mat.(x).(y+i) > !h then h := highest_mat.(x).(y+i)
		done;
		blocks_mat.(x).(y).(!h) <- axis, len;
		for i = 0 to len - 1 do
			highest_mat.(x).(y+i) <- !h + 1
		done
	|'x' ->
		let h = ref 0 in
		for i = 0 to len - 1 do
			if highest_mat.(x+i).(y) > !h then h := highest_mat.(x+i).(y)
		done;
		blocks_mat.(x).(y).(!h) <- axis, len;
		for i = 0 to len - 1 do
			highest_mat.(x+i).(y) <- !h + 1
		done
	|_ -> failwith "axis error"
;;

let rec remove_rep l =
	match l with
	|[] -> []
	|h::t -> if List.mem h t then remove_rep t else h::(remove_rep t)
;;

let rec remove_n n l =
	match l with
	|[] -> []
	|c::t when c = n -> remove_n n t
	|h::t -> h::remove_n n t
;;

let find_parents n full_mat =
	let l = ref [] in
	for k = 0 to Array.length full_mat.(0).(0) - 2 do
		for i = 0 to Array.length full_mat - 1 do
			for j = 0 to Array.length full_mat.(0) - 1 do
				if full_mat.(i).(j).(k) = n then l := full_mat.(i).(j).(k+1) :: !l
			done
		done
	done;
	remove_rep (remove_n n (remove_n 0 !l))
;;

let find_children n full_mat =
	let l = ref [] in
	for k = 1 to Array.length full_mat.(0).(0) - 1 do
		for i = 0 to Array.length full_mat - 1 do
			for j = 0 to Array.length full_mat.(0) - 1 do
				if full_mat.(i).(j).(k) = n then l := full_mat.(i).(j).(k-1) :: !l
			done
		done
	done;
	remove_rep (remove_n n (remove_n 0 !l))
;;

let fill_blocks mat =
	let x, y, z = Array.length mat, Array.length mat.(0), Array.length mat.(0).(0) in
	let full = Array.init x (fun i -> Array.init y (fun j -> Array.init z (fun k -> 0))) in
	let current_n = ref 1 in
	for i = 0 to x - 1 do
		for j = 0 to y - 1 do
			for k = 0 to z - 1 do
				let axis, dir = mat.(i).(j).(k) in
				if (axis, dir) <> ('x', 0) then
					match axis with
					|'x' -> for p = i to i + dir - 1 do full.(p).(j).(k) <- !current_n done; current_n := !current_n + 1
					|'y' -> for p = j to j + dir - 1 do full.(i).(p).(k) <- !current_n done; current_n := !current_n + 1
					|_ -> for p = k to k + dir - 1 do full.(i).(j).(p) <- !current_n done; current_n := !current_n + 1
			done
		done
	done;
	full
;;

let print_full zmax full_mat =
	for i = 0 to zmax - 1 do
		print_int i;
		print_string "\n";
		for j = 0 to Array.length full_mat - 1 do
			for k = 0 to Array.length full_mat.(0) - 1 do
				let c = full_mat.(j).(k).(i) in
				if c = 0 then
					print_string ". "
				else begin
					print_int c;
					print_string " "
				end
			done;
			print_string "\n"
		done;
		print_string "\n"
	done
;;

let rec fall_count n g fall_l =
	let parents = fst g.(n-1) in
	List.iter (fun p ->
		let children = snd g.(p-1) in
		let will_fall = ref true in
		List.iter (fun c -> if not (List.mem c !fall_l) then will_fall := false) children;
		if !will_fall then begin
			fall_l := p :: !fall_l;
			fall_count p g fall_l
		end
	) parents
;;

let print_tab tab =
	Array.iter (fun e -> print_int e; print_string "; ") tab;
	print_string "\n"
;;

let print_graph g =
	for i = 0 to Array.length g - 1 do
		print_int (i+1);
		print_string ": parents ";
		let parents, children = g.(i) in
		List.iter (fun p -> print_int p; print_string ", ") parents;
		print_string " ; children ";
		List.iter (fun c -> print_int c; print_string ", ") children;
		print_string "\n"
	done
;;

let _ =
(*parse*)
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let tab1 = Array.init (List.length input_list - 1) (fun i -> Array.of_list (String.split_on_char '~' (List.nth input_list i))) in
	let tab2 = Array.map (fun a -> Array.of_list (String.split_on_char ',' a.(0) @ String.split_on_char ',' a.(1))) tab1 in
	let coords_tab = Array.map (fun a -> [|iof a.(0); iof a.(1); iof a.(2); iof a.(3); iof a.(4); iof a.(5)|]) tab2 in
	let xmax, ymax, zmax = ref 1, ref 1, ref 1 in
	Array.iter (fun a -> if a.(3)+1 > !xmax then xmax := a.(3)+1; if a.(4)+1 > !ymax then ymax := a.(4)+1; if a.(5)+1 > !zmax then zmax := a.(5)+1) coords_tab;
	let blocks_mat = Array.init !xmax (fun i -> Array.init !ymax (fun j -> Array.init !zmax (fun k -> ('x', 0)))) in
	Array.iter (fun a ->
		match a with
		|[|x1;y1;z1;x2;y2;z2|] ->
			if x1 <> x2 then blocks_mat.(x1).(y1).(z1) <- 'x', x2 - x1 + 1
			else if y1 <> y2 then blocks_mat.(x1).(y1).(z1) <- 'y', y2 - y1 + 1
			else if z1 <> z2 then blocks_mat.(x1).(y1).(z1) <- 'z', z2 - z1 + 1
			else blocks_mat.(x1).(y1).(z1) <- 'x', 1
		|_ -> failwith "error"
	) coords_tab;

(*falling*)
	let highest_mat = Array.init !xmax (fun i -> Array.init !ymax (fun i -> 0)) in
	for i = 0 to !zmax - 1 do
		for j = 0 to !xmax - 1 do
			for k = 0 to !ymax - 1 do
				let (axis, len) = blocks_mat.(j).(k).(i) in
				if (axis, len) <> ('x', 0) then begin
					blocks_mat.(j).(k).(i) <- 'x', 0;
					place_lowest j k axis len highest_mat blocks_mat
				end
			done
		done
	done;

(*build complete array*)
	let highest_point = ref 0 in
	Array.iter (fun a -> Array.iter (fun n -> if n > !highest_point then highest_point := n) a) highest_mat;
	let full_space = fill_blocks blocks_mat in

	print_string "\n";
	print_full !highest_point full_space;

(*build graph*)
	let graph = Array.init (Array.length coords_tab) (fun i -> [], []) in
	let added = ref [] in
	for i = 0 to !highest_point - 1 do
		for j = 0 to !xmax - 1 do
			for k = 0 to !ymax - 1 do
				let n = full_space.(j).(k).(i) in
				if n <> 0 && not (List.mem n !added) then begin
					added := n :: !added;
					let parents = find_parents n full_space in
					let children = find_children n full_space in
					graph.(n-1) <- parents, children;
				end
			done
		done
	done;
	print_graph graph;
	print_string "\n";

(*get leaves*)
	let leaves = ref [] in
	for i = 0 to Array.length graph - 1 do
		let n = i+1 in
		let parents = fst graph.(i) in
		if parents = [] then leaves := n :: !leaves
		else begin
			let flag = ref true in
			List.iter (fun m -> if List.length (snd graph.(m-1)) < 2 then flag := false) parents;
			if !flag then leaves := n :: !leaves
		end
	done;

(*compute*)
	let result = ref 0 in
	for i = 0 to Array.length coords_tab - 1 do
		let n = i+1 in
		print_int n;
		print_string " : ";
		if not (List.mem n !leaves) then begin
			let fall_list = ref [n] in
			fall_count n graph fall_list;
			fall_list := remove_n n !fall_list;
			List.iter (fun m -> print_int m; print_string ", ") !fall_list;
			result := !result + List.length !fall_list
		end;
		print_string "\n";
	done;

	print_int !result;
	print_string " total blocks would fall\n"
;;
