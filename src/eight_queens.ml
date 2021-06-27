let rec rec_calculate cache (board : Board.t) =
  if Board.left board = 0 then
    let s = Board.to_string board in
    if Hashtbl.mem cache s then
      ()
    else
      let _ =
        Board.to_all_variant_strings board
        |> List.iter (fun s -> Hashtbl.replace cache s 0)
      in
      let _ = Hashtbl.find cache "count" + 1 |> Hashtbl.replace cache "count" in
      Board.print board
  else
    for i = 0 to board.n - 1 do
      for j = 0 to board.n - 1 do
        match Board.place board i j with
        | Some new_board -> rec_calculate cache new_board
        | None -> ()
      done
    done

let caculate n =
  if n < 1 then
    failwith "invalid input"
  else if n = 1 then
    Printf.printf "\n+---+\n| Q |\n+---+\n\nfundamental: 1    all: 1\n"
  else if n = 2 || n = 3 then
    Printf.printf "\nfundamental: 0    all: 0\n"
  else
    let cache = Hashtbl.create 1 in
    let _ = Hashtbl.add cache "count" 0 in

    let board = Board.make n in
    let half = float_of_int n /. 2. |> ceil |> int_of_float |> fun i -> i - 1 in
    (* only put the first queen at the half of the upper left quarter *)
    for i = 0 to half do
      for j = i to half do
        match Board.place board i j with
        | Some new_board -> rec_calculate cache new_board
        | None -> failwith "the first placement should never fail"
      done
    done;
    Printf.printf "\nfundamental: %d    all: %d\n"
      (Hashtbl.find cache "count")
      (Hashtbl.length cache - 1)

let () =
  let n =
    if Array.length Sys.argv < 2 then
      8
    else
      Sys.argv.(1) |> int_of_string
  in
  caculate n
