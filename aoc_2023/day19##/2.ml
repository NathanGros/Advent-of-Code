let parse input_tab flows_tab =
	for i = 0 to Array.length flows_tab - 1 do
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

let combinations (xmin, xmax) (mmin, mmax) (amin, amax) (smin, smax) =
	(xmax - xmin + 1) * (mmax - mmin + 1) * (amax - amin + 1) * (smax - smin + 1)
;;

let rec compute res flow_name flows_tab (xmin, xmax) (mmin, mmax) (amin, amax) (smin, smax) =
(*get flow variables*)
	let instructions_ref = ref [] in
	let else_flow_ref = ref "" in
	for i = 0 to Array.length flows_tab - 1 do
		match flows_tab.(i) with
		|name, instruct, else_f -> if name = flow_name then begin instructions_ref := instruct; else_flow_ref := else_f end
	done;
	let instruct = Array.of_list !instructions_ref in
	let else_flow = !else_flow_ref in
(*real computing*)
	let xm, xM, mm, mM, am, aM, sm, sM = ref xmin, ref xmax, ref mmin, ref mmax, ref amin, ref amax, ref smin, ref smax in
	for i = 0 to Array.length instruct - 1 do
		let c, op, n, out = instruct.(i) in
		match c, op with
		|'x', '<' ->
			if !xm < n then begin
				if out = "A" then
					res := !res + (combinations (!xm, n - 1) (!mm, !mM) (!am, !aM) (!sm, !sM))
				else if out <> "R" then
					compute res out flows_tab (!xm, n - 1) (!mm, !mM) (!am, !aM) (!sm, !sM)
			end;
			xm := n
		|'x', '>' -> 
			if !xM > n then begin
				if out = "A" then
					res := !res + (combinations (n + 1, !xM) (!mm, !mM) (!am, !aM) (!sm, !sM))
				else if out <> "R" then
					compute res out flows_tab (n + 1, !xM) (!mm, !mM) (!am, !aM) (!sm, !sM)
			end;
			xM := n
		|'m', '<' -> 
			if !mm < n then begin
				if out = "A" then
					res := !res + (combinations (!xm, !xM) (!mm, n - 1) (!am, !aM) (!sm, !sM))
				else if out <> "R" then
					compute res out flows_tab (!xm, !xM) (!mm, n - 1) (!am, !aM) (!sm, !sM)
			end;
			mm := n
		|'m', '>' -> 
			if !mM > n then begin
				if out = "A" then
					res := !res + (combinations (!xm, !xM) (n + 1, !mM) (!am, !aM) (!sm, !sM))
				else if out <> "R" then
					compute res out flows_tab (!xm, !xM) (n + 1, !mM) (!am, !aM) (!sm, !sM)
			end;
			mM := n
		|'a', '<' -> 
			if !am < n then begin
				if out = "A" then
					res := !res + (combinations (!xm, !xM) (!mm, !mM) (!am, n - 1) (!sm, !sM))
				else if out <> "R" then
					compute res out flows_tab (!xm, !xM) (!mm, !mM) (!am, n - 1) (!sm, !sM)
			end;
			am := n
		|'a', '>' -> 
			if !aM > n then begin
				if out = "A" then
					res := !res + (combinations (!xm, !xM) (!mm, !mM) (n + 1, !aM) (!sm, !sM))
				else if out <> "R" then
					compute res out flows_tab (!xm, !xM) (!mm, !mM) (n + 1, !aM) (!sm, !sM)
			end;
			aM := n
		|'s', '<' -> 
			if !sm < n then begin
				if out = "A" then
					res := !res + (combinations (!xm, !xM) (!mm, !mM) (!am, !aM) (!sm, n - 1))
				else if out <> "R" then
					compute res out flows_tab (!xm, !xM) (!mm, !mM) (!am, !aM) (!sm, n - 1)
			end;
			sm := n
		|'s', '>' -> 
			if !sM > n then begin
				if out = "A" then
					res := !res + (combinations (!xm, !xM) (!mm, !mM) (!am, !aM) (n + 1, !sM))
				else if out <> "R" then
					compute res out flows_tab (!xm, !xM) (!mm, !mM) (!am, !aM) (n + 1, !sM)
			end;
			sM := n
		|_, _ -> failwith "flow error"
	done;
	if else_flow = "A" then
		res := !res + (combinations (!xm, !xM) (!mm, !mM) (!am, !aM) (!sm, !sM))
	else if else_flow <> "R" then
		compute res else_flow flows_tab (!xm, !xM) (!mm, !mM) (!am, !aM) (!sm, !sM)
;;

let _ =
(*parse*)
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	close_in file;
	let input_list = String.split_on_char '\n' input_string in
	let input_tab = Array.of_list input_list in
	let flows_nb = ref 0 in
	Array.iter (fun s -> if s <> "" && String.get s 0 <> '{' then flows_nb := !flows_nb + 1) input_tab;
	let flows_tab = (Array.init !flows_nb (fun i -> "", [('e', 'e', 0, "")], "")) in
	parse input_tab flows_tab;

(*print*)
	for i = 0 to Array.length flows_tab - 1 do
		print_flow flows_tab.(i)
	done;
	print_string "\n";

(*computing*)
	let result = ref 0 in
	compute result "in" flows_tab (1, 4000) (1, 4000) (1, 4000) (1, 4000);
	print_int !result;
	print_string "\n"
;;
