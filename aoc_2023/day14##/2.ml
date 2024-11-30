let parse l =
	let n = List.length l - 1 in
	let m = match l with |[] -> 0 |t::q -> String.length t in
	let tab = Array.make_matrix n m ' ' in
	let rec aux l tab i =
		match l with
		|[] -> ()
		|[x] -> ()
		|t::q ->
			for j = 0 to String.length t - 1 do
				tab.(i).(j) <- String.get t j
			done;
			aux q tab (i+1)
	in
	aux l tab 0;
	tab
;;


let tilt_north tab =
	for i = 0 to Array.length tab - 1 do
		for j = 0 to Array.length tab.(0) - 1 do
			if tab.(i).(j) = 'O' then begin
				let k = ref (i - 1) in
				while (!k >= 0) && (tab.(!k).(j) = '.') do
					k := !k - 1
				done;
				k := !k + 1;
				tab.(i).(j) <- '.';
				tab.(!k).(j) <- 'O'
			end
		done
	done
;;


let tilt_west tab =
	for j = 0 to Array.length tab.(0) - 1 do
		for i = 0 to Array.length tab - 1 do
			if tab.(i).(j) = 'O' then begin
				let k = ref (j - 1) in
				while (!k >= 0) && (tab.(i).(!k) = '.') do
					k := !k - 1
				done;
				k := !k + 1;
				tab.(i).(j) <- '.';
				tab.(i).(!k) <- 'O'
			end
		done
	done
;;


let tilt_south tab =
	for i = Array.length tab - 1 downto 0 do
		for j = 0 to Array.length tab.(0) - 1 do
			if tab.(i).(j) = 'O' then begin
				let k = ref (i + 1) in
				while (!k <= (Array.length tab - 1)) && (tab.(!k).(j) = '.') do
					k := !k + 1
				done;
				k := !k - 1;
				tab.(i).(j) <- '.';
				tab.(!k).(j) <- 'O'
			end
		done
	done
;;


let tilt_east tab =
	for j = Array.length tab.(0) - 1 downto 0 do
		for i = 0 to Array.length tab - 1 do
			if tab.(i).(j) = 'O' then begin
				let k = ref (j + 1) in
				while (!k <= (Array.length tab.(0) - 1)) && (tab.(i).(!k) = '.') do
					k := !k + 1
				done;
				k := !k - 1;
				tab.(i).(j) <- '.';
				tab.(i).(!k) <- 'O'
			end
		done
	done
;;


let cycle tab =
	tilt_north tab;
	tilt_west tab;
	tilt_south tab;
	tilt_east tab
;;


let weight tab =
	let result = ref 0 in
	for i = 0 to Array.length tab - 1 do
		for j = 0 to Array.length tab.(0) - 1 do
			if tab.(i).(j) = 'O' then
				result := !result + (Array.length tab - i)
		done
	done;
	!result
;;


let copy_matrix tab =
	let n = Array.length tab in
	let m = Array.length tab.(0) in
	let tab2 = Array.make_matrix n m '.' in
	for i = 0 to n - 1 do
		for j = 0 to m - 1 do
			tab2.(i).(j) <- tab.(i).(j)
		done
	done;
	tab2
;;


let print_matrix tab =
	for i = 0 to Array.length tab - 1 do
		for j = 0 to Array.length tab.(0) - 1 do
			print_string (String.make 1 tab.(i).(j));
		done;
		print_string "\n"
	done
;;


let cycles_compare original_tab tab n =
	let cycles_tab = Array.init n (fun f -> [||]) in
	for i = 0 to n - 1 do
		cycles_tab.(i) <- copy_matrix tab;
		cycle tab
	done;
	for i = 1 to n - 1 do
		for j = 0 to i - 1 do
			if cycles_tab.(i) = cycles_tab.(j) then begin
				let diff = i - j in
				let equivalent = 1000000000 mod diff in
				for k = 1 to equivalent + 20 * diff do
					cycle original_tab
				done;
				print_matrix original_tab;
				print_string "\n";
				print_int (weight original_tab);
			end
		done
	done
;;


let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let tab = parse input_list in
	let original_tab = copy_matrix tab in
	cycles_compare original_tab tab 152;
	print_string "\n"
