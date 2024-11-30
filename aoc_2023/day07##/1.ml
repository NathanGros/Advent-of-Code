type nature =
	|Five
	|Four
	|Full
	|Three
	|Pairs
	|Two
	|One
;;


let rec print_list l =
	match l with
	|[] -> print_string "\n"
	|t::q -> print_int t; print_string " "; print_list q
;;


let rec parse l tab i =
	match l with
	|[] -> ()
	|[x] -> ()
	|t::q ->
		let line = String.split_on_char ' ' t in
		let hand, bid =
			match line with
			|h::b::[] -> h, int_of_string b
			|_ -> failwith "parse error"
		in
		tab.(i) <- (hand, bid);
		parse q tab (i+1)
;;


let find_nature hand =
	let l = ref [] in
	for i = 0 to 4 do
		l := !l @ [String.get hand i]
	done;
	let rec pull_elt l x =
		match l with
		|[] -> [], 0
		|t::q when t = x -> fst (pull_elt q x), snd (pull_elt q x) + 1
		|t::q -> t::(fst (pull_elt q x)), snd (pull_elt q x)
	in
	let fst_elt l =
		match l with
		|[] -> failwith "fst_elt error"
		|t::q -> t
	in
	let result_l = ref [] in
	while !l <> [] do
		let temp = pull_elt !l (fst_elt !l) in
		l := fst temp;
		result_l := !result_l @ [snd temp]
	done;
	match !result_l with
	|[5] -> Five
	|[4;1] |[1;4] -> Four
	|[3;2] |[2;3] -> Full
	|[3;1;1] |[1;3;1] |[1;1;3] -> Three
	|[2;2;1] |[2;1;2] |[1;2;2] -> Pairs
	|[2;1;1;1] |[1;2;1;1] |[1;1;2;1] |[1;1;1;2] -> Two
	|[1;1;1;1;1] -> One
	|_ -> failwith "nature error"
;;


let compare_values c1 c2 = (*true if c1 > c2*)
	match c1, c2 with
	|'A', _ -> true
	|_, 'A' -> false
	|'K', _ -> true
	|_, 'K' -> false
	|'Q', _ -> true
	|_, 'Q' -> false
	|'J', _ -> true
	|_, 'J' -> false
	|'T', _ -> true
	|_, 'T' -> false
	|'9', _ -> true
	|_, '9' -> false
	|'8', _ -> true
	|_, '8' -> false
	|'7', _ -> true
	|_, '7' -> false
	|'6', _ -> true
	|_, '6' -> false
	|'5', _ -> true
	|_, '5' -> false
	|'4', _ -> true
	|_, '4' -> false
	|'3', _ -> true
	|_, '3' -> false
	|'2', _ -> true
	|_, '2' -> false
	|_ -> failwith "weird"
;;


let rec compare_chars hand1 hand2 i =
	if i > 4 then
		false
	else
		if (String.get hand1 i) <> (String.get hand2 i) then
			compare_values (String.get hand1 i) (String.get hand2 i)
		else
			compare_chars hand1 hand2 (i+1)
;;


let compare hand1 hand2 = (*true if hand1 > hand2*)
	let nat1 = find_nature hand1 in
	let nat2 = find_nature hand2 in
	match nat1, nat2 with
	|a, b when a = b ->
		compare_chars hand1 hand2 0
	|Five, _ -> true
	|_, Five -> false
	|Four, _ -> true
	|_, Four -> false
	|Full, _ -> true
	|_, Full -> false
	|Three, _ -> true
	|_, Three -> false
	|Pairs, _ -> true
	|_, Pairs -> false
	|Two, _ -> true
	|_, Two -> false
	|_ -> failwith "huh"
;;


let sorted tab =
	let flag = ref true in
	for i = 0 to Array.length tab - 2 do
		if not (compare (fst tab.(i)) (fst tab.(i+1))) then
			flag := false
	done;
	!flag
;;


let sort tab =
	while not (sorted tab) do
		for i = 0 to Array.length tab - 2 do
			if not (compare (fst tab.(i)) (fst tab.(i+1))) then
				let temp = tab.(i) in
				tab.(i) <- tab.(i+1);
				tab.(i+1) <- temp;
		done
	done
;;


let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;

	let tab = Array.init (List.length input_list - 1) (fun f -> ("", 0)) in
	parse input_list tab 0;

	sort tab;

	let n = Array.length tab in
	let result = ref 0 in
	for i = 0 to n - 1 do
		print_string (fst tab.(i));
		print_string " ";
		print_int (snd tab.(i));
		print_string "\n";
		result := !result + (n-i) * snd tab.(i)
	done;
	print_string "\n";
	print_int !result;
	print_string "\n"

