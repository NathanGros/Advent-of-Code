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


let rec next_coord tab (x1, y1) arrival_dir i =
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
		i + 1
	else
		next_coord tab (x2, y2) dir (i+1)
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

	let sx = ref 0 in
	let sy = ref 0 in
	for i = 0 to n - 1 do
		for j = 0 to m - 1 do
			if input_tab.(i).(j) = 'S' then begin
				sx := i;
				sy := j
			end
		done
	done;

	let result = (next_coord input_tab (!sx, (!sy - 1)) East 1) / 2 in
	print_int result;
	print_string "\n"
