type module_type =
	|Flip of bool
	|Conj of (string * bool) list
;;

let print_inputs l =
	List.iter (fun (labelfrom, label, pulse) -> print_string labelfrom; print_string " : "; if pulse then print_string "high -> " else print_string "low -> "; print_string label; print_string "\n") l
;;

let print_tab t = 
	Array.iter (fun s -> print_endline s) t
;;

let remove_space s =
	if String.get s 0 = ' ' then String.sub s 1 (String.length s - 1) else s
;;

let rec remove_none l =
	match l with
	|[] -> []
	|("none", "none", false)::t -> remove_none t
	|h::t -> h::remove_none t
;;

let make_broadcast_input s =
	let l = String.split_on_char ',' (String.sub s 15 (String.length s - 15)) in
	let nospace_l = List.map (fun s -> remove_space s) l in
	List.map (fun label -> "broadcaster", label, false) nospace_l
;;

let update_module modules_states label pulse =
	let index = ref 0 in
	for i = 0 to Array.length modules_states - 1 do
		if fst modules_states.(i) = label then index := i
	done;
	match snd modules_states.(!index) with
	|Flip(b) ->
		if not pulse then begin
			modules_states.(!index) <- label, Flip(not b);
			if b then
				0
			else
				1
		end
		else -1
	|Conj(l) ->
		let flag = ref true in
		List.iter (fun (_, b) -> if not b then flag := false) l;
		if !flag then
			0
		else
			1
;;

let rec process modules modules_states pending low_nb high_nb =
	match pending with
	|[] -> ()
	|(labelfrom, label, pulse)::t ->
		print_inputs pending;
		print_string "\n";
		let index = ref (-1) in
		for i = 0 to Array.length modules_states - 1 do
			if snd (fst modules.(i)) = label then index := i
		done;
		if !index >= 0 then begin
			let ((m, _), links) = modules.(!index) in
			if m = '&' then begin
				let l = match modules_states.(!index) with |_, Conj(lvar) -> lvar |_ -> failwith "process error" in
				let newl = List.map (fun (n, b) -> if n = labelfrom then (n, pulse) else (n, b)) l in
				modules_states.(!index) <- label, Conj(newl);
			end;
			let outpulse = update_module modules_states label pulse in
			let newinputs = List.init (List.length links) (fun i ->
				if outpulse = 0 then begin
					low_nb := !low_nb + 1;
					label, (List.nth links i), false
				end
				else if outpulse = 1 then begin
					high_nb := !high_nb + 1;
					label, (List.nth links i), true
				end
				else
					"none", "none", false
			) in
			process modules modules_states (t @ (remove_none newinputs)) low_nb high_nb
		end
		else 
			process modules modules_states t low_nb high_nb
;;

let _ =
(*parse*)
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let input_tab_broadcast = Array.init (List.length input_list - 1) (fun i -> List.nth input_list i) in
	let broadcast_s = ref "" in
	let offset = ref 0 in
	let input_tab = Array.init (Array.length input_tab_broadcast - 1) (fun i -> "") in
	for i = 0 to Array.length input_tab_broadcast - 1 do
		if List.hd (String.split_on_char '>' input_tab_broadcast.(i)) = "broadcaster -" then begin
			offset := 1;
			broadcast_s := input_tab_broadcast.(i)
		end
		else
			input_tab.(i - !offset) <- input_tab_broadcast.(i)
	done;

(*formatting*)
	let modules = Array.map (fun s ->
		let m = String.get s 0 in
		let label = String.sub s 1 2 in
		let links = String.split_on_char ',' (String.sub s 7 (String.length s - 7)) in
		(m, label), List.map (fun s -> remove_space s) links
	) input_tab in
	let modules_states = Array.map (fun ((m, label), links) ->
		if m = '%' then label, Flip(false)
		else begin
			let l = ref [] in
			for i = 0 to Array.length modules - 1 do
				let (m2, label2), links2 = modules.(i) in
				if (List.mem label links2) then l := (label2, false) :: !l
			done;
			label, Conj(!l)
		end
	) modules in
	
	Array.iter (fun (label, e) ->
		match (label, e) with
		|s, Flip(b) -> print_string s; print_string " : "; if b then print_string "1\n" else print_string "0\n"
		|s, Conj(l) -> print_string s; print_string " : "; List.iter (fun (s2, b) -> print_string s2; print_string ": "; if b then print_string "1" else print_string "0"; print_string ", ") l; print_string "\n"
	) modules_states;

(*processing*)
	let low_nb = ref 0 in
	let high_nb = ref 0 in
	let broadcast_input = make_broadcast_input !broadcast_s in
	for i = 0 to 999 do
		print_string "\n\nProcessing :\n\nbutton : low -> broadcaster\n\n";
		low_nb := !low_nb + 1 + (List.length broadcast_input);
		process modules modules_states broadcast_input low_nb high_nb;
		print_string "\nLow pulses : ";
		print_int !low_nb;
		print_string ", high pulses : ";
		print_int !high_nb;
		print_string "\nResult : ";
		print_int (!low_nb * !high_nb);
		print_string "\n"
	done
;;
