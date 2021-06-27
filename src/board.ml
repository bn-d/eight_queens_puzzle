(* 0 - empty
   1 - queen
   2 - blocked *)
type t = { board : int array; n : int }

let make n = { board = Array.make (n * n) 0; n }

let index n i j = (n * i) + j

let set b n i j v = Array.set b (index n i j) v

let place t i j =
  let n = t.n in
  if Array.get t.board (index n i j) = 0 then (
    let board = Array.copy t.board in

    if i < n - 1 then (
      (* block col *)
      for ii = i + 1 to n - 1 do
        set board n ii j 2
      done;
      (* block dia *)
      for ii = i + 1 to n - 1 do
        let diff = i - ii in
        let j1 = j + diff in
        let j2 = j - diff in
        if j1 >= 0 && j1 < n then set board n ii j1 2;
        if j2 >= 0 && j2 < n then set board n ii j2 2
      done);
    set board n i j 1;
    Some { board; n })
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
      if Array.get t.board index = 1 then
        Printf.printf " Q "
      else
        Printf.printf "   ";
      Printf.printf "|"
    done;
    Printf.printf "\n%s\n" h_line
  done

let to_string t =
  String.init (t.n * t.n) (fun idx -> if t.board.(idx) = 1 then 'Q' else '-')

let to_all_variant_strings t =
  let n = t.n in
  let size = n * n in
  let b_000 = to_string t in
  let b_090 =
    String.init size (fun idx ->
        let i = idx / n and j = idx mod n in
        b_000.[n - 1 - i + (n * j)])
  in
  let b_180 = String.init size (fun idx -> b_000.[size - idx - 1]) in
  let b_270 = String.init size (fun idx -> b_090.[size - idx - 1]) in

  let r_000 =
    String.init size (fun idx ->
        let i = idx / n and j = idx mod n in
        b_000.[((i + 1) * n) - 1 - j])
  in
  let r_090 =
    String.init size (fun idx ->
        let i = idx / n and j = idx mod n in
        b_090.[((i + 1) * n) - 1 - j])
  in
  let r_180 = String.init size (fun idx -> r_000.[size - idx - 1]) in
  let r_270 = String.init size (fun idx -> r_090.[size - idx - 1]) in

  [ b_000; b_090; b_180; b_270; r_000; r_090; r_180; r_270 ]
