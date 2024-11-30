type trilean =
	|True
	|False
	|Unknown
;;


let parse l =
	let n = List.length l - 1 in
	let rec find_lengths l i =
		match l with
		|[] -> 0
		|[x] -> 0
		|t::q -> if t = "" then i else find_lengths q (i+1)
	in
	let m = find_lengths l 0 in
	let part_tab = Array.init (n-m-1) (fun f -> (0, 0, 0, 0)) in
	let flow_tab = Array.init m (fun f -> ("", "")) in

	let rec aux l part_tab flow_tab m i =
		match l with
		|[] -> ()
		|[x] -> ()
		|t::q ->
			if t = "" then aux q part_tab flow_tab m (i+1)
			else
				if String.get t 0 = '{' then begin (*part*)
					let start_pos = ref 0 in
					let end_pos = ref 0 in
					let value = ref 0 in
					for j = 1 to String.length t - 1 do
						if (String.get t j = 'x' || String.get t j = 'm' || String.get t j = 'a' || String.get t j = 's') then (*detect start*)
							start_pos := (j+2)
						;
						if (String.get t j = ',' || String.get t j = '}') then begin (*detect end and fill part_tab*)
							let xtab, mtab, atab, stab = match part_tab.(i-m-1) with
							|(x, m, a, s) -> x, m, a, s
							in
							end_pos := (j-1);
							value := int_of_string (String.sub t !start_pos (!end_pos - !start_pos + 1));
							if String.get t (!start_pos - 2) = 'x' then
								part_tab.(i-m-1) <- (!value, mtab, atab, stab)
							;
							if String.get t (!start_pos - 2) = 'm' then
								part_tab.(i-m-1) <- (xtab, !value, atab, stab)
							;
							if String.get t (!start_pos - 2) = 'a' then
								part_tab.(i-m-1) <- (xtab, mtab, !value, stab)
							;
							if String.get t (!start_pos - 2) = 's' then
								part_tab.(i-m-1) <- (xtab, mtab, atab, !value)
						end
					done;
				end
				else (*flow*)
					for j = 0 to String.length t - 1 do
					if String.get t j = '{' then
						flow_tab.(i) <- (String.sub t 0 j, String.sub t (j+1) (String.length t - j - 2))
					done
				;
			aux q part_tab flow_tab m (i+1)
	in
	aux l part_tab flow_tab m 0;
	part_tab, flow_tab
;;


let tril_to_bool x =
	match x with
	|True -> true
	|False -> false
	|Unknown -> false
;;


let find_flow_index flow_tab label =
	let index = ref 0 in
	for i = 0 to Array.length flow_tab - 1 do
		if fst flow_tab.(i) = label then
			index := i
	done;
	!index
;;


let rec is_accepted flow_tab (x, m, a, s) flow_index res =
	let conds = snd flow_tab.(flow_index) in
	let processed = ref false in
	print_int 7;
	print_string "\n";
	for i = 0 to String.length conds - 1 do
		match String.sub conds i 2, !processed with
		|"x>", false -> print_string "x1";
				let k = ref (i+2) in
				while String.get conds !k <> ':' do
					k := !k + 1
				done;
				let nb = int_of_string (String.sub conds (i+2) (!k-(i+2))) in
				let j = ref (!k+1) in
				while String.get conds !k <> ',' do
					j := !j + 1
				done;
				let output_flow = String.sub conds (!k+1) (!j-(!k+1)) in
				print_string output_flow;
				print_string " ";
				if x > nb then begin
					processed := true;
					if output_flow = "A" then
						res := True
					else if output_flow = "R" then
						res := False
					else
						res := is_accepted flow_tab (x, m, a, s) (find_flow_index flow_tab output_flow) res
				end
		|"x<", false -> print_string "x"
		|"m>", false -> print_string "m"
		|"m<", false -> print_string "m"
		|"a>", false -> print_string "a"
		|"a<", false -> print_string "a"
		|"s>", false -> print_string "s"
		|"s<", false -> print_string "s";
				let k = ref (i+2) in
				while String.get conds !k <> ':' do
					k := !k + 1
				done;
				let nb = int_of_string (String.sub conds (i+2) (!k-(i+2))) in
				let j = ref (!k+1) in
				while String.get conds !k <> ',' do
					j := !j + 1
				done;
				let output_flow = String.sub conds (!k+1) (!j-(!k+1)) in
				print_string output_flow;
				print_string " ";
				if s < nb then begin
					processed := true;
					if output_flow = "A" then
						res := True
					else if output_flow = "R" then
						res := False
					else
						res := is_accepted flow_tab (x, m, a, s) (find_flow_index flow_tab output_flow) res
				end
		|_, false -> if String.get conds i = ',' then begin
		if String.length conds - 1 - i < 6 then
			let output_flow = String.sub conds (i+1) (String.length conds - 2 - i) in
			if output_flow = "A" then
				res := True
			else if output_flow = "R" then
				res := False
			else
				res := is_accepted flow_tab (x, m, a, s) (find_flow_index flow_tab output_flow) res
		end
	done;
	!res
;;


let print_parts tab =
	for i = 0 to Array.length tab - 1 do
		let xtab, mtab, atab, stab = match tab.(i) with
		|(x, m, a, s) -> x, m, a, s
		in
		print_int xtab;
		print_string " ";
		print_int mtab;
		print_string " ";
		print_int atab;
		print_string " ";
		print_int stab;
		print_string "\n"
	done
;;


let print_flows tab =
	for i = 0 to Array.length tab - 1 do
		let label, definition = match tab.(i) with
		|(l, d) -> l, d
		in
		print_string label;
		print_string " ";
		print_string definition;
		print_string "\n"
	done
;;


let _ =
(*parse*)
	let file = open_in "test_input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let part_tab, flow_tab = parse input_list in

	print_parts part_tab;
	print_string "\n\n";
	print_flows flow_tab;
	print_string "\n";
	print_string "\n";

(*script*)
	let start_flow = ref 0 in
	for i = 0 to Array.length flow_tab - 1 do
		if fst flow_tab.(i) = "in" then start_flow := i
	done;

	let result = ref 0 in

	for i = 0 to Array.length part_tab - 1 do
		if tril_to_bool (is_accepted flow_tab part_tab.(i) !start_flow (ref Unknown)) then begin
			let xtab, mtab, atab, stab = match part_tab.(i) with
			|(x, m, a, s) -> x, m, a, s
			in
			result := !result + xtab + mtab + atab + stab;
			print_string "\n\n"
		end
	done;

	print_int !result;
	print_string "\n"
