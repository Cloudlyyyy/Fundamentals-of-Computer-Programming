(* klotski.ml: core functionality of the Klotski game. *)
(* Student name: Cloudly Ceen           *)
(* CMS cluster login name:      *)

(* ---------------------------------------------------------------------- 
 * Type
 * ---------------------------------------------------------------------- *)

type loc = int * int
type dir = Up | Down | Left | Right
type move = char * dir * int

module LocM =
  struct
    type t = loc
    let compare = Stdlib.compare
  end

module LocSet : Set.S with type elt = loc = Set.Make(LocM)

(* Sets of LocSets.  Used locally only. *)

module LocSetM =
  struct
    type t = LocSet.t
    let compare = LocSet.compare
  end

module LocSetSet = Set.Make(LocSetM)

module CharM =
  struct
    type t = char
    let compare = Stdlib.compare
  end

module CharMap : Map.S with type key = char = Map.Make(CharM)

type piece = LocSet.t
type t = { pieces : piece CharMap.t ; unoccupied : LocSet.t }

(* ---------------------------------------------------------------------- 
 * Functions.
 * ---------------------------------------------------------------------- *)

(* Create a board from a string. *)
let read s = 
  let rec iter p u r c =
    match () with
      | _ when r = 5 -> { pieces = p; unoccupied = u }
      | _ when c = 4 -> iter p u (r + 1) 0 
      | _ -> 
        let i = r * 4 + c in
        let ch = s.[i] in
          if ch = '.'  (* unoccupied location; add to unoccupied set *)
            then iter p (LocSet.add (r, c) u) r (c + 1)
            else  (* occupied; add to appropriate piece set *)
              try
                let cs  = CharMap.find ch p in     (* old piece set *)
                let cs' = LocSet.add (r, c) cs in  (* add new location *)
                let p'  = CharMap.add ch cs' p in  (* store back into map *)
                  iter p' u r (c + 1)
              with
                Not_found ->  (* new piece; create a new piece set *)
                  let cs = LocSet.singleton (r, c) in
                  let p' = CharMap.add ch cs p in
                    iter p' u r (c + 1)
  in
    if String.length s <> 20
      then failwith "read: invalid input string length"
      else iter CharMap.empty LocSet.empty 0 0

(* Convert the board to a string representation suitable for printing. *)
let show b = 
  let string_of_char_list = function
    | [a;b;c;d] -> Printf.sprintf "%c%c%c%c" a b c d
    | _ -> failwith "invalid char list"
  in
  let char_at board loc =
    let rec iter = function
      | [] -> raise Not_found
      | (c, locs) :: t -> 
        if LocSet.mem loc locs then c else iter t
    in
    if LocSet.mem loc board.unoccupied
      then '.'
      else iter (CharMap.bindings board.pieces)
  in
  (String.concat "\n"
     (List.map (fun r ->
        string_of_char_list
          (List.map (char_at b) 
            (List.map (fun c -> (r, c)) [0; 1; 2; 3])))
        [0; 1; 2; 3; 4])) ^ "\n"


let is_solved b =
  if CharMap.exists (fun _ ls -> LocSet.equal ls (LocSet.of_list [(3,1);(3,2);(4,1);(4,2)])) b.pieces
    then true 
      else false

let compare b1 b2 =
  let get_piece (_, piece) = piece in 
  if LocSet.equal b1.unoccupied b2.unoccupied then
    let set_piece1 = LocSetSet.of_list (List.map get_piece (CharMap.bindings b1.pieces)) 
    and set_piece2 = LocSetSet.of_list (List.map get_piece (CharMap.bindings b2.pieces)) in 
        if LocSetSet.equal set_piece1 set_piece2 
          then 0 
            else LocSetSet.compare set_piece1 set_piece2
  else LocSet.compare b1.unoccupied b2.unoccupied 
 
let remove c ({ pieces = p; unoccupied = u } as b) = 
  if CharMap.mem c p then 
    {pieces = CharMap.remove c p; unoccupied = LocSet.union (CharMap.find c p) u}
  else b

let add (c, p) { pieces = ps; unoccupied = u } = 
  if CharMap.mem c ps then 
    None 
    else if LocSet.subset p u
          then Some {pieces = CharMap.add c p ps; unoccupied = LocSet.diff u p} 
            else None

(* val make_move : move -> t -> t option *)

let make_move (c, d, i) b =
  let change_pos (a,b) = 
    match d with 
      | Up -> (a-1,b)
      | Down -> (a+1,b)
      | Left -> (a,b-1)
      | Right -> (a,b+1) 
  in 
  let poss_pos pos unoccupied = LocSet.subset pos unoccupied in 
  let rec iter count curr_pos = 
    if poss_pos curr_pos (LocSet.union (CharMap.find c b.pieces) b.unoccupied)
      then 
        if count = 0 
          then add (c, curr_pos) (remove c b)
            else iter (count-1) (LocSet.map change_pos curr_pos)
        else None 
      in
  if (CharMap.mem c b.pieces) = false || i < 1 then 
    None 
    else iter i (CharMap.find c b.pieces)

let next b =
  let get_poss_move (c,p) = 
    let rec max_poss_move d i lst = 
      match make_move (c,d,i) b with 
        | None -> lst
        | Some board -> max_poss_move d (i+1) (board::lst)
    in List.concat [max_poss_move Up 1 []; max_poss_move Down 1 []; 
          max_poss_move Left 1 []; max_poss_move Right 1 []]
  in List.concat (List.map get_poss_move (CharMap.bindings b.pieces))

(* Function to interactively test the "next" function. 
 * Useful for debugging. *)
let test_next b =
  let bs = next b in
    begin
      print_string (show b ^ "\n");
      List.iter 
        (fun b -> print_string ("----\n\n" ^ show b ^ "\n"))
        bs
    end

