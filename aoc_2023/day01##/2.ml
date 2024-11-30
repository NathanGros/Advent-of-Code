let detect_number str i = (*returns number if there is a letter spelled number starting from i*)
	let n = String.length str in
	if n-i < 5 then None else
		match String.get str i with
		|'z' -> if String.sub str i 4 = "zero" then Some 0 else None
		|'o' -> if String.sub str i 3 = "one" then Some 1 else None
		|'t' -> if String.sub str i 3 = "two" then Some 2 else if String.sub str i 5 = "three" then Some 3 else None
		|'f' -> if String.sub str i 4 = "four" then Some 4 else if String.sub str i 4 = "five" then Some 5 else None
		|'s' -> if String.sub str i 3 = "six" then Some 6 else if String.sub str i 5 = "seven" then Some 7 else None
		|'e' -> if String.sub str i 5 = "eight" then Some 8 else None
		|'n' -> if String.sub str i 4 = "nine" then Some 9 else None
		|_ -> None
;;


let _ =
	let file = open_in "input.txt" in
	let input_string = really_input_string file (in_channel_length file) in
	close_in file;

	let result = ref 0 in
	let number1 = ref 0 in
	let number2 = ref 0 in
	let flag = ref true in (*true if no number encountered yet*)

	for i = 0 to String.length input_string - 1 do
		match String.get input_string i with
		|'\n' ->
			result := !result + 10 * !number1 + !number2;
			flag := true
		|'0' -> if !flag then (number1 := 0; number2 := 0; flag := false) else number2:= 0
		|'1' -> if !flag then (number1 := 1; number2 := 1; flag := false) else number2:= 1
		|'2' -> if !flag then (number1 := 2; number2 := 2; flag := false) else number2:= 2
		|'3' -> if !flag then (number1 := 3; number2 := 3; flag := false) else number2:= 3
		|'4' -> if !flag then (number1 := 4; number2 := 4; flag := false) else number2:= 4
		|'5' -> if !flag then (number1 := 5; number2 := 5; flag := false) else number2:= 5
		|'6' -> if !flag then (number1 := 6; number2 := 6; flag := false) else number2:= 6
		|'7' -> if !flag then (number1 := 7; number2 := 7; flag := false) else number2:= 7
		|'8' -> if !flag then (number1 := 8; number2 := 8; flag := false) else number2:= 8
		|'9' -> if !flag then (number1 := 9; number2 := 9; flag := false) else number2:= 9
		|c -> let nb = match detect_number input_string i with
							|Some x -> x
							|None -> -1
				in
				if not (nb = -1) then
					if !flag then (number1 := nb; number2 := nb; flag := false) else number2:= nb
	done;

	print_int !result;
	print_string "\n"
