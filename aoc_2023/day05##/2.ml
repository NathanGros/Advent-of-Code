let ascii_print_characters_nb = 140;;

let print_seeds_tab tab =
	for i = 0 to Array.length tab - 1 do
		print_int (fst tab.(i));
		print_string " + ";
		print_int (snd tab.(i));
		print_string " = ";
		print_int (fst tab.(i) + snd tab.(i));
		print_string "\n"
	done;
	print_string "\n"
;;

let rec merge_seed_range seeds_list =
	match seeds_list with
	|[] -> []
	|h1::h2::t -> (h1, h2)::(merge_seed_range t)
	|_ -> failwith "parse error"
;;

let draw char_nb largest tab =
	let char_value = float_of_int largest /. float_of_int char_nb in
	for i = 0 to Array.length tab - 1 do
		let startpos = float_of_int (fst tab.(i)) /. char_value in
		let endpos = float_of_int (fst tab.(i) + snd tab.(i)) /. char_value in
		for j = 0 to int_of_float startpos - 1 do
			print_string "."
		done;
		for j = int_of_float startpos to int_of_float endpos do
			print_string "#"
		done;
		for j = int_of_float endpos + 1 to char_nb do
			print_string "."
		done;
		print_string "\n"
	done
;;

let draw_print char_nb largest tab =
	for i = 0 to Array.length tab - 1 do
		let start, range = tab.(i) in
		print_int start;
		print_string ", ";
		print_int range;
		print_string "\n"
	done
;;

let is_united tab = (*yes i know future me, that's bad*)
	let res = ref true in
	for i = 0 to Array.length tab - 2 do
		let gi, di = fst tab.(i), fst tab.(i) + snd tab.(i) - 1 in
		for j = (i + 1) to Array.length tab - 1 do
			let gj, dj = fst tab.(j), fst tab.(j) + snd tab.(j) - 1 in
			let overlap = gi < gj && di >= gj || gi >= gj && gi <= dj in
			if overlap then res := false
		done
	done;
	!res
;;

let unite tab =
	let tab2 = ref (Array.init (Array.length tab) (fun i -> tab.(i))) in
	while not (is_united !tab2) do
		for i = 0 to Array.length !tab2 - 2 do
			let gi, di = fst !tab2.(i), fst !tab2.(i) + snd !tab2.(i) - 1 in
			for j = (i + 1) to Array.length !tab2 - 1 do
				let gj, dj = fst !tab2.(j), fst !tab2.(j) + snd !tab2.(j) - 1 in
				let overlap = (gi < gj && di >= gj) || (gi >= gj && gi <= dj) in
				if overlap then begin
					let most_left = min gi gj in
					let new_range = max di dj - most_left in
					let tab3 = !tab2 in
					tab3.(i) <- (most_left, new_range);
					tab3.(j) <- (0, 0);
					tab2 := tab3
				end
			done
		done
	done;
	let count_null = ref 0 in
	for i = 0 to Array.length !tab2 - 1 do
		if !tab2.(i) = (0, 0) then count_null := !count_null + 1
	done;
	let res_tab = Array.init (Array.length !tab2 - !count_null) (fun i -> (0, 0)) in
	let offset = ref 0 in
	for i = 0 to Array.length !tab2 - 1 do
		if !tab2.(i) <> (0, 0) then
			res_tab.(i - !offset) <- !tab2.(i)
		else
			offset := !offset + 1
	done;
	res_tab
;;

let split_categories tab =
	let res = Array.init 7 (fun i -> [||]) in
	let last_i = ref 0 in
	let n = ref 0 in
	for i = 0 to Array.length tab - 1 do
		if tab.(i) = "" then begin
			res.(!n) <- Array.init (i - !last_i) (fun j -> tab.(!last_i + j));
			last_i := i + 2;
			n := !n + 1
		end;
	done;
	res
;;

let print_split_cat tab =
	for i = 0 to Array.length tab - 1 do
		List.iter (print_endline) tab.(i);
		print_string "\n"
	done
;;

let largest tab =
	let res = ref 0 in
	for i = 0 to Array.length tab - 1 do
		let temp = fst tab.(i) + snd tab.(i) in
		if temp > !res then res := temp;
	done;
	!res
;;

let make_convert_tab tab1 =
	let tab2 = Array.init (Array.length tab1) (fun i -> (0, 0, 0)) in
	for i = 0 to Array.length tab1 - 1 do
		let l = List.map int_of_string (String.split_on_char ' ' tab1.(i)) in
		let dest, src, range = match l with d::s::r::[] -> d, s, r |_ -> failwith "error" in
		tab2.(i) <- dest, src, range
	done;
	tab2
;;

let convert convert_tab src_tab =
	let res_list = ref [] in
	for i = 0 to Array.length src_tab - 1 do
		let is_converted = ref false in
		for j = 0 to Array.length convert_tab - 1 do
			let gi, di = fst src_tab.(i), fst src_tab.(i) + snd src_tab.(i) - 1 in
			let dest, src, range = convert_tab.(j) in
			let gj, dj = src, src + range - 1 in
			(*case one*)
			if gi >= gj && di <= dj then begin
				is_converted := true;
				let delta = dest - src in
				let new_gi = gi + delta in
				let new_range = di - gi + 1 in
				res_list := (new_gi, new_range)::!res_list;
				src_tab.(i) <- (0, 0)
			end;
			(*case two*)
			if gi < gj && di > dj then begin
				is_converted := true;
				res_list := (dest, range)::!res_list;
				res_list := (gi, gj - gi)::!res_list;
				res_list := (dj + 1, di - dj)::!res_list
			end;
			(*case three*)
			if gi >= gj && di > dj && dj >= gi then begin
				is_converted := true;
				let delta = dest - src in
				let new_gi = gi + delta in
				let new_di = dj + delta in
				res_list := (new_gi, new_di - new_gi + 1)::!res_list;
				src_tab.(i) <- (dj + 1, di - dj)
			end;
			(*case four*)
			if di <= dj && gi < gj && gj <= di then begin
				is_converted := true;
				res_list := (dest, di - gj + 1)::!res_list;
				src_tab.(i) <- (gi, gj - gi)
			end;
		done;
		if not !is_converted || src_tab.(i) <> (0, 0) then
			let gi, di = fst src_tab.(i), fst src_tab.(i) + snd src_tab.(i) - 1 in
			res_list := (gi, di - gi + 1)::!res_list
	done;
	Array.of_list (List.rev !res_list)
;;

let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;

	let seeds_list = merge_seed_range (List.map int_of_string (List.tl (String.split_on_char ' ' (List.hd input_list)))) in
	let seeds_tab = unite (Array.of_list seeds_list) in
	let largest_seed = largest seeds_tab in

(*draw everything*)
	print_string "seeds :\n";
	draw ascii_print_characters_nb largest_seed seeds_tab;

	let convert_tab_string = split_categories (Array.of_list (List.tl (List.tl (List.tl input_list)))) in (*Array of convert data lists*)
	let convert_seeds_tab = make_convert_tab convert_tab_string.(0) in (*Array of (destination, source, range)*)
	let soils_tab = unite (convert convert_seeds_tab seeds_tab) in (*to test without unite*)
	let largest_soil = largest soils_tab in
	print_string "reachable soils :\n";
	draw ascii_print_characters_nb largest_soil soils_tab;

	let convert_soils_tab = make_convert_tab convert_tab_string.(1) in
	let fertilizers_tab = unite (convert convert_soils_tab soils_tab) in
	let largest_fertilizer = largest fertilizers_tab in
	print_string "reachable fertilizers :\n";
	draw ascii_print_characters_nb largest_fertilizer fertilizers_tab;

	let convert_fertilizers_tab = make_convert_tab convert_tab_string.(2) in
	let waters_tab = unite (convert convert_fertilizers_tab fertilizers_tab) in
	let largest_water = largest waters_tab in
	print_string "reachable waters :\n";
	draw ascii_print_characters_nb largest_water waters_tab;

	let convert_waters_tab = make_convert_tab convert_tab_string.(3) in
	let lights_tab = unite (convert convert_waters_tab waters_tab) in
	let largest_light = largest lights_tab in
	print_string "reachable lights :\n";
	draw ascii_print_characters_nb largest_light lights_tab;

	let convert_lights_tab = make_convert_tab convert_tab_string.(4) in
	let temperatures_tab = unite (convert convert_lights_tab lights_tab) in
	let largest_temperature = largest temperatures_tab in
	print_string "reachable temperatures :\n";
	draw ascii_print_characters_nb largest_temperature temperatures_tab;

	let convert_temperatures_tab = make_convert_tab convert_tab_string.(5) in
	let humiditys_tab = unite (convert convert_temperatures_tab temperatures_tab) in
	let largest_humidity = largest humiditys_tab in
	print_string "reachable humidities :\n";
	draw ascii_print_characters_nb largest_humidity humiditys_tab;

	let convert_humiditys_tab = make_convert_tab convert_tab_string.(6) in
	let locations_tab = unite (convert convert_humiditys_tab humiditys_tab) in
	let largest_location = largest locations_tab in
	print_string "reachable locations :\n";
	draw ascii_print_characters_nb largest_location locations_tab;

(*final result*)
	let lowest_location = ref (largest locations_tab) in
	for i = 0 to Array.length locations_tab - 1 do
		let temp = fst locations_tab.(i) in
		if temp < !lowest_location then lowest_location := temp;
	done;

	print_string "\nThe lowest location is ";
	print_int !lowest_location;
	print_string "\n"
;;
