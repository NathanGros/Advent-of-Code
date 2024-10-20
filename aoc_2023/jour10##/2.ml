type direction =
	|East
	|South
	|North
	|West
;;


let rec parse l tab i =
	match l with
	|[] -> ()
	|t::q ->
		for j = 0 to String.length t - 1 do
			tab.(i).(j) <- String.get t j
		done;
		parse q tab (i+1)
;;


let invert_dir dir =
	match dir with
	|East -> West
	|West -> East
	|North -> South
	|South -> North
;;


let print_tab tab =
	let n = Array.length tab in
	let m = Array.length tab.(0) in
	for i = 0 to n - 1 do
		for j = 0 to m - 1 do
			print_string (String.make 1 tab.(i).(j))
		done;
		print_string "\n"
	done
;;


let rec flow tab x y c =
	if x > 0 && x < (Array.length tab - 1) && y > 0 && y < (Array.length tab.(0) - 1) then
		if not (tab.(x).(y) = '@' || tab.(x).(y) = 'I' || tab.(x).(y) = 'T' || tab.(x).(y) = 'O') then begin
			tab.(x).(y) <- c;
			flow tab x (y-1) c;
			flow tab x (y+1) c;
			flow tab (x-1) y c;
			flow tab (x+1) y c
		end
;;


let rec mark_loop tab (x1, y1) arrival_dir i =
	let x2, y2, dir =
		match tab.(x1).(y1) with
		|'-' -> if arrival_dir = East then x1, (y1-1), (invert_dir West) else x1, (y1+1), (invert_dir East)
		|'|' -> if arrival_dir = South then (x1-1), y1, (invert_dir North) else (x1+1), y1, (invert_dir South)
		|'7' -> if arrival_dir = West then (x1+1), y1, (invert_dir South) else x1, (y1-1), (invert_dir West)
		|'J' -> if arrival_dir = West then (x1-1), y1, (invert_dir North) else x1, (y1-1), (invert_dir West)
		|'L' -> if arrival_dir = East then (x1-1), y1, (invert_dir North) else x1, (y1+1), (invert_dir East)
		|'F' -> if arrival_dir = East then (x1+1), y1, (invert_dir South) else x1, (y1+1), (invert_dir East)
		|_ -> failwith "out_of_loop"
	in
	if tab.(x2).(y2) = 'S' then
		tab.(x2).(y2) <- '@'
	else begin
		tab.(x1).(y1) <- '@';
		mark_loop tab (x2, y2) dir (i+1)
	end
;;


let place_bigger tab1 tab2 x y =
	match tab1.(x).(y) with
	|'-' ->
		tab2.(3*x).(3*y) <- '.';
		tab2.(3*x).(3*y+1) <- '.';
		tab2.(3*x).(3*y+2) <- '.';
		tab2.(3*x+1).(3*y) <- '-';
		tab2.(3*x+1).(3*y+1) <- '-';
		tab2.(3*x+1).(3*y+2) <- '-';
		tab2.(3*x+2).(3*y) <- '.';
		tab2.(3*x+2).(3*y+1) <- '.';
		tab2.(3*x+2).(3*y+2) <- '.'
	|'|' ->
		tab2.(3*x).(3*y) <- '.';
		tab2.(3*x).(3*y+1) <- '|';
		tab2.(3*x).(3*y+2) <- '.';
		tab2.(3*x+1).(3*y) <- '.';
		tab2.(3*x+1).(3*y+1) <- '|';
		tab2.(3*x+1).(3*y+2) <- '.';
		tab2.(3*x+2).(3*y) <- '.';
		tab2.(3*x+2).(3*y+1) <- '|';
		tab2.(3*x+2).(3*y+2) <- '.'
	|'L' ->
		tab2.(3*x).(3*y) <- '.';
		tab2.(3*x).(3*y+1) <- '|';
		tab2.(3*x).(3*y+2) <- '.';
		tab2.(3*x+1).(3*y) <- '.';
		tab2.(3*x+1).(3*y+1) <- 'L';
		tab2.(3*x+1).(3*y+2) <- '-';
		tab2.(3*x+2).(3*y) <- '.';
		tab2.(3*x+2).(3*y+1) <- '.';
		tab2.(3*x+2).(3*y+2) <- '.'
	|'J' ->
		tab2.(3*x).(3*y) <- '.';
		tab2.(3*x).(3*y+1) <- '|';
		tab2.(3*x).(3*y+2) <- '.';
		tab2.(3*x+1).(3*y) <- '-';
		tab2.(3*x+1).(3*y+1) <- 'J';
		tab2.(3*x+1).(3*y+2) <- '.';
		tab2.(3*x+2).(3*y) <- '.';
		tab2.(3*x+2).(3*y+1) <- '.';
		tab2.(3*x+2).(3*y+2) <- '.'
	|'7' ->
		tab2.(3*x).(3*y) <- '.';
		tab2.(3*x).(3*y+1) <- '.';
		tab2.(3*x).(3*y+2) <- '.';
		tab2.(3*x+1).(3*y) <- '-';
		tab2.(3*x+1).(3*y+1) <- '7';
		tab2.(3*x+1).(3*y+2) <- '.';
		tab2.(3*x+2).(3*y) <- '.';
		tab2.(3*x+2).(3*y+1) <- '|';
		tab2.(3*x+2).(3*y+2) <- '.'
	|'F' ->
		tab2.(3*x).(3*y) <- '.';
		tab2.(3*x).(3*y+1) <- '.';
		tab2.(3*x).(3*y+2) <- '.';
		tab2.(3*x+1).(3*y) <- '.';
		tab2.(3*x+1).(3*y+1) <- 'F';
		tab2.(3*x+1).(3*y+2) <- '-';
		tab2.(3*x+2).(3*y) <- '.';
		tab2.(3*x+2).(3*y+1) <- '|';
		tab2.(3*x+2).(3*y+2) <- '.'
	|'.' ->
		tab2.(3*x).(3*y) <- '.';
		tab2.(3*x).(3*y+1) <- '.';
		tab2.(3*x).(3*y+2) <- '.';
		tab2.(3*x+1).(3*y) <- '.';
		tab2.(3*x+1).(3*y+1) <- '.';
		tab2.(3*x+1).(3*y+2) <- '.';
		tab2.(3*x+2).(3*y) <- '.';
		tab2.(3*x+2).(3*y+1) <- '.';
		tab2.(3*x+2).(3*y+2) <- '.'
	|'S' ->
		tab2.(3*x).(3*y) <- '.';
		tab2.(3*x).(3*y+1) <- '|';
		tab2.(3*x).(3*y+2) <- '.';
		tab2.(3*x+1).(3*y) <- '-';
		tab2.(3*x+1).(3*y+1) <- 'S';
		tab2.(3*x+1).(3*y+2) <- '.';
		tab2.(3*x+2).(3*y) <- '.';
		tab2.(3*x+2).(3*y+1) <- '.';
		tab2.(3*x+2).(3*y+2) <- '.'
;;


let expand_tab tab n m =
	let tab2 = Array.make_matrix (3*n) (3*m) '.' in
	for i = 0 to n - 1 do
		for j = 0 to m - 1 do
			place_bigger tab tab2 i j
		done
	done;
	tab2
;;


let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	let input_list = String.split_on_char '\n' input_string in
	close_in file;
	let n = List.length input_list - 1 in
	let m =
		match input_list with
		|[] -> 0
		|t::q -> String.length t
	in
	let input_tab = Array.make_matrix n m '.' in
	parse input_list input_tab 0;
	
	let big_tab = expand_tab input_tab n m in
	let sx = ref 0 in
	let sy = ref 0 in
	for i = 0 to 3*n - 1 do
		for j = 0 to 3*m - 1 do
			if big_tab.(i).(j) = 'S' then begin
				sx := i;
				sy := j
			end
		done
	done;

	mark_loop big_tab (!sx, (!sy - 1)) East 1; (* /!\ input specific /!\ *)
	big_tab.(!sx-1).(!sy) <- '@';
	flow big_tab 101 101 'I';
	print_tab big_tab;

	let result = ref 0 in
	for i = 0 to 3*n - 1 do
		for j = 0 to 3*m - 1 do
			if (i mod 3 = 1) && (j mod 3 = 1) then begin
				if big_tab.(i).(j) = 'I' then
					result := !result + 1
			end
		done
	done;
	print_int !result;
	print_string "\n"
