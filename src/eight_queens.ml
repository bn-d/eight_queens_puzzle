let n = 4

let rec rec_calculate (board : Board.t) =
  if Board.left board = 0 then
    Board.print board
  else
    for i = 0 to board.n - 1 do
      for j = 0 to board.n - 1 do
        match Board.place board i j with
        | Some new_board -> rec_calculate new_board
        | None -> ()
      done
    done

let caculate n =
  let board = Board.make n in
  let _ = ignore board in
  let half = float_of_int n /. 2. |> ceil |> int_of_float |> fun i -> i - 1 in
  for i = 0 to half do
    for j = i to half do
      match Board.place board i j with
      | Some new_board -> rec_calculate new_board
      | None -> failwith "the first placement should never fail"
    done
  done

let () = caculate n
