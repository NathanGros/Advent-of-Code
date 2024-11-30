let int_of_char c =
   match c with
   | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
      int_of_string (String.make 1 c)
   | '.' -> -1
   | _ -> -2

let rec make_input_matrix mat l n =
   match l with
   | [] -> ()
   | str :: q ->
      for i = 0 to String.length str - 1 do
         mat.(n).(i) <- int_of_char (String.get str i)
      done;
      make_input_matrix mat q (n + 1)

let touch_special i k mat =
   let flag = ref false in
   if i > 0 then if mat.(i - 1).(k) = -2 then flag := true;
   if i < 139 then if mat.(i + 1).(k) = -2 then flag := true;
   if k > 0 then if mat.(i).(k - 1) = -2 then flag := true;
   if k < 139 then if mat.(i).(k + 1) = -2 then flag := true;
   if i > 0 && k > 0 then if mat.(i - 1).(k - 1) = -2 then flag := true;
   if i > 0 && k < 139 then if mat.(i - 1).(k + 1) = -2 then flag := true;
   if i < 139 && k > 0 then if mat.(i + 1).(k - 1) = -2 then flag := true;
   if i < 139 && k < 139 then if mat.(i + 1).(k + 1) = -2 then flag := true;
   !flag

let _ =
   let file = open_in "input.txt" in
   let input_string = really_input_string file (in_channel_length file) in
   let input_list = String.split_on_char '\n' input_string in
   close_in file;
   let input_matrix = Array.make_matrix 140 140 (-1) in
   make_input_matrix input_matrix input_list 0;
   let result = ref 0 in

   for i = 0 to 139 do
      for j = 0 to 139 do
         if not (input_matrix.(i).(j) = -1 || input_matrix.(i).(j) = -2) then
            if
            j = 0
               || input_matrix.(i).(j - 1) = -1
               || input_matrix.(i).(j - 1) = -2
            then (
               (*debut du nombre*)
               let k = ref j in
               let read_number = ref 0 in
               let flag = ref false in
               while
               !k < 140
               && not (input_matrix.(i).(!k) = -1 || input_matrix.(i).(!k) = -2)
               do
                  read_number := (!read_number * 10) + input_matrix.(i).(!k);
                  if touch_special i !k input_matrix then flag := true;
                  k := !k + 1
               done;
               if !flag then result := !result + !read_number)
      done
   done;
   print_int !result;
   print_string "\n"
