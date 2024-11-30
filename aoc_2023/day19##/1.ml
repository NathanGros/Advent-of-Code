let parse input_tab flows_tab parts_tab =
	let in_flows = ref true in
	let parts_i = ref 0 in
	for i = 0 to Array.length input_tab - 2 do
		if !in_flows && input_tab.(i) = "" then in_flows := false
		else
			if !in_flows then begin
				let l = String.split_on_char '{' input_tab.(i) in
				let flow = List.hd l in
				let instructions_str = Array.of_list (String.split_on_char ',' (String.sub (List.nth l 1) 0 (String.length (List.nth l 1) - 1))) in
				let else_flow = instructions_str.(Array.length instructions_str - 1) in
				let conditions = Array.to_list (Array.init (Array.length instructions_str - 1) (fun j ->
					let s = instructions_str.(j) in
					let l2 = String.split_on_char ':' s in
					let fst_elt = List.nth l2 0 in
					let c = String.get fst_elt 0 in
					let comp = String.get fst_elt 1 in
					let n = int_of_string (String.sub fst_elt 2 (String.length fst_elt - 2)) in
					let out_flow = List.nth l2 1 in
					(c, comp, n, out_flow)
				)) in
				flows_tab.(i) <- flow, conditions, else_flow
			end
			else begin
				let s = input_tab.(i) in
				let l = String.split_on_char ',' (String.sub s 1 (String.length s - 2)) in
				let part = [|0; 0; 0; 0|] in
				for j = 0 to 3 do
					let s2 = List.nth l j in
					part.(j) <- int_of_string (String.sub s2 2 (String.length s2 - 2))
				done;
				parts_tab.(!parts_i) <- part;
				parts_i := !parts_i + 1
			end
	done
;;

let print_flow (name, instructions, else_flow) =
	print_string name;
	print_string " : [";
	for i = 0 to List.length instructions - 1 do
		let c, cond, n, out_flow = List.nth instructions i in
		print_string (String.make 1 c);
		print_string (String.make 1 cond);
		print_int n;
		print_string " -> ";
		print_string out_flow;
		print_string " ; "
	done;
	print_string "], else -> ";
	print_string else_flow;
	print_string "\n"
;;

let print_part a =
	print_string "x=";
	print_int a.(0);
	print_string ", m=";
	print_int a.(1);
	print_string ", a=";
	print_int a.(2);
	print_string ", s=";
	print_int a.(3);
	print_string "\n"
;;

let rec is_accepted part flow_name flows_tab =
	let x, m, a, s = part.(0), part.(1), part.(2), part.(3) in
	let instructions_ref = ref [] in
	let else_flow_ref = ref "" in
	for i = 0 to Array.length flows_tab - 1 do
		match flows_tab.(i) with
		|name, instruct, else_f -> if name = flow_name then begin instructions_ref := instruct; else_flow_ref := else_f end
	done;
	let instruct = Array.of_list !instructions_ref in
	let else_flow = !else_flow_ref in
	let not_found = ref true in
	let out_flow = ref "" in
	for i = 0 to Array.length instruct - 1 do
		let c, op, n, out = instruct.(i) in
		match c, op with
		|'x', '<' -> if x < n && !not_found then (not_found := false; out_flow := out)
		|'x', '>' -> if x > n && !not_found then (not_found := false; out_flow := out)
		|'m', '<' -> if m < n && !not_found then (not_found := false; out_flow := out)
		|'m', '>' -> if m > n && !not_found then (not_found := false; out_flow := out)
		|'a', '<' -> if a < n && !not_found then (not_found := false; out_flow := out)
		|'a', '>' -> if a > n && !not_found then (not_found := false; out_flow := out)
		|'s', '<' -> if s < n && !not_found then (not_found := false; out_flow := out)
		|'s', '>' -> if s > n && !not_found then (not_found := false; out_flow := out)
	done;
	if !out_flow = "" then
		match else_flow with
		|"A" -> true
		|"R" -> false
		|other -> is_accepted part other flows_tab
	else
		match !out_flow with
		|"A" -> true
		|"R" -> false
		|other -> is_accepted part other flows_tab
;;

let _ =
(*parse*)
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	close_in file;
	let input_list = String.split_on_char '\n' input_string in
	let input_tab = Array.of_list input_list in
	let flows_nb = ref 0 in
	let parts_nb = ref 0 in
	Array.iter (fun s -> if s <> "" then if String.get s 0 = '{' then parts_nb := !parts_nb + 1 else flows_nb := !flows_nb + 1) input_tab;
	let flows_tab = (Array.init !flows_nb (fun i -> "", [('e', 'e', 0, "")], "")) in
	let parts_tab = (Array.init !parts_nb (fun i -> [||])) in
	parse input_tab flows_tab parts_tab;

(*print*)
	for i = 0 to Array.length flows_tab - 1 do
		print_flow flows_tab.(i)
	done;
	print_string "\n";
	for i = 0 to Array.length parts_tab - 1 do
		print_part parts_tab.(i)
	done;
	print_string "\n";

(*computing*)
	let result = ref 0 in
	Array.iter (fun p -> if is_accepted p "in" flows_tab then result := !result + p.(0) + p.(1) + p.(2) + p.(3)) parts_tab;
	print_int !result;
	print_string "\n"
;;
