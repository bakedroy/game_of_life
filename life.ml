let print_matrix mtrx =
        let rec print_array arr = 
                let print_element = function
                        0 -> print_string "- "
                        | n -> print_string "0 "
                in
                Array.iter print_element arr;
                Printf.printf "\n"
        in
        Array.iter print_array mtrx;
        print_newline ()

let print_separator width =
        let rec sub_print_separator = function 
                 0 -> print_newline ()
                | n -> print_char '-'; sub_print_separator (n-1)
        in
        sub_print_separator width

let calculate_N mtrx i j =
        mtrx.(i-1).(j-1) + mtrx.(i-1).(j) + mtrx.(i-1).(j+1) +
        mtrx.(i).(j-1)   +                  mtrx.(i).(j+1) +
        mtrx.(i+1).(j-1) + mtrx.(i+1).(j) + mtrx.(i+1).(j+1)

let calculate_new_C mtrx i j =
        let n = calculate_N mtrx i j in
        if mtrx.(i).(j) = 1 then
                if n = 2 || n = 3 then  1
                else                    0
        else
                if n = 3 then           1
                else                    0

let change_generation mtrx row_size column_size tmp_mtrx = 
        (* Create new large matrix. *)
        for i = 0 to row_size - 1 do
                for j = 0 to column_size - 1 do
                        tmp_mtrx.(i+1).(j+1) <- mtrx.(i).(j)
                done;
        done;
        for i = 0 to row_size - 1 do
                for j = 0 to column_size - 1 do
                        mtrx.(i).(j) <- calculate_new_C tmp_mtrx (i+1) (j+1)
                done;
        done;
        ()

let game width height =
        let tmp_mtrx = Array.make_matrix (height + 2) (width + 2) 0 in
        let canvas = Array.make_matrix height width 0 in
        let init_canvas () =
                let max = (height * width / 2)in
                let rec _sub_init_canvas i max =
                        let p_x = (Random.int width) in
                        let p_y = (Random.int height) in
                        match max - i with
                        0   -> ()
                        | _ -> canvas.(p_y).(p_x) <- 1; 
                               _sub_init_canvas (i + 1) max
                in
                _sub_init_canvas 0 max
        in
        let rec periodical_print_matrix count =
                ignore (Sys.command "clear");
                print_matrix canvas;
                (* print_separator width; *)
                (* Thread.delay 0.1; *)
                change_generation canvas height width tmp_mtrx;
                print_endline (string_of_int count);
                Unix.sleep 1;
                periodical_print_matrix (count+1)
        in
        Random.init (int_of_float (Unix.time ()));
        init_canvas ();
        periodical_print_matrix 0

let main () =
        if Array.length Sys.argv = 3 then
                let width = (int_of_string Sys.argv.(1)) in
                let height =(int_of_string Sys.argv.(2)) in
                if width > 10 && height > 10 then
                        game width height
                else
                        print_endline "Width and height must be gte 10."
        else
                let width = 20 in
                let height = 20 in
                game width height

let _ = main ();

