(* 0 - empty
   1 - queen
   2 - blocked *)
type t = { board : char array; n : int; left : int }

let make n = { board = Array.make (n * n) '\000'; n; left = n }

let index t i j = (t.n * i) + j

let get t i j = Array.get t.board (index t i j)

let left t = t.left

let place t i j =
  let cell = get t i j in
  if cell = '\000' then (
    let board = Array.copy t.board in

    let set b i j v = Array.set b (index t i j) v in
    (* block row *)
    for jj = 0 to t.n - 1 do
      set board i jj '\002'
    done;
    (* block col *)
    for ii = 0 to t.n - 1 do
      set board ii j '\002'
    done;
    (* block dia *)
    for ii = 0 to t.n - 1 do
      let diff = i - ii in
      let j1 = j + diff in
      let j2 = j - diff in
      if j1 >= 0 && j1 < t.n then set board ii j1 '\002';
      if j2 >= 0 && j2 < t.n then set board ii j2 '\002'
    done;
    set board i j '\001';
    Some { board; n = t.n; left = t.left - 1 })
  else
    None

let print t =
  let h_line =
    String.init ((4 * t.n) + 1) (fun i -> if i mod 4 == 0 then '+' else '-')
  in
  Printf.printf "\n%s\n" h_line;

  for i = 0 to t.n - 1 do
    Printf.printf "|";
    for index = i * t.n to (i * t.n) + t.n - 1 do
      if Array.get t.board index = '\001' then
        Printf.printf " Q "
      else
        Printf.printf "   ";
      Printf.printf "|"
    done;
    Printf.printf "\n%s\n" h_line
  done
