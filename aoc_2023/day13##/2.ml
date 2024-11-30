let rec parse tab l n = (*parsing*)
	match l with
	|[] -> tab
	|""::t -> parse tab t (n+1)
	|h::t -> tab.(n) <- h::tab.(n); parse tab t n
;;

let transpose tab = (*transpose matrix but on string array*)
	Array.init (String.length tab.(0)) (fun i -> String.init (Array.length tab) (fun j -> String.get tab.(j) i))
;;

let is_mirror_smudge tab n =
	let delta = min n (Array.length tab - n) in
	let no_match_count = ref 0 in
	let res = ref 0 in
	for i = 0 to delta - 1 do
		if tab.(n - i - 1) <> tab.(n + i) then begin
			no_match_count := !no_match_count + 1;
			res := i
		end
	done;
	if !no_match_count <> 1 then
		false
	else begin
		let smudge_count = ref 0 in
		for j = 0 to String.length tab.(0) - 1 do
			if String.get tab.(n - !res - 1) j <> String.get tab.(n + !res) j then smudge_count := !smudge_count + 1
		done;
		!smudge_count = 1
	end
;;

let find_mirror_smudge tab =
	let found_mirror = ref 0 in
	for i = 0 to Array.length tab - 1 do
		if is_mirror_smudge tab i then found_mirror := i
	done;
	!found_mirror
;;

let count_symmetries p = (*formula as defined in the challenge*)
	find_mirror_smudge (transpose p) + 100 * find_mirror_smudge p
;;

let _ =
	(*parse : make patterns_tab which is an array of patterns (arrays of strings)*)
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	let input_tab = Array.of_list input_list in
	let count_patterns = ref 0 in
	Array.iter (fun x -> if x = "" then count_patterns := !count_patterns + 1) input_tab;
	let patterns_tab_temp = parse (Array.init !count_patterns (fun i -> [])) input_list 0 in
	let patterns_tab = Array.map (fun l -> Array.of_list (List.rev l)) patterns_tab_temp in
	close_in file;

	let result = ref 0 in
	Array.iter (fun p -> result := !result + count_symmetries p) patterns_tab;

	(*print result*)
	print_string "Symmetries summary : ";
	print_int !result;
	print_string "\n"
;;
