(* A.1 factorial *)

(*
  FRAME 0 (initial environment)
    parent: none
    bindings:
      - : [primitive function -]
      * : [primitive function *] 

  FUNCTION 0 (let fun n -> ... )
    env: FRAME 0
    param: n
    body:
    let rec iter m r = 
      if m = 0 
        then r 
          else iter (m - 1) (r * m)
        in iter n 1

  FRAME 1 (let factorial = FUNCTION 0 )
    parent: FRAME 0
    bindings:
    factorial : FUNCTION 0

  FRAME 2 (FUNCTION 0 applied to factorial 3)
    parent: FRAME 0
    bindings:
      n : 3

  FUNCTION 1 (let fun m r -> )
    env: FRAME 2
    param: m r 
    body: 
    if m = 0
      then r
        else iter (m - 1) (r * m)

  FRAME 3 (let rec iter = FUNCTION 1)
    parent: FRAME 2
    bindings:
    iter : FUNCTION 1

  FRAME 4 (FUNCTION 1 applied to 3 and 1)
  parent: FRAME 3
  bindings:
      m : 3 
      r : 1 

  FRAME 5 (FUNCTION 1 applied to 2 and 3)
  parent: FRAME 3 
  bindings:
      m : 2 
      r : 3

  FRAME 6 (FUNCTION 1 applied to 1 6)
  parent: FRAME 3 
  bindings:
      m : 1
      r : 6

  FRAME 7 (FUNCTION 1 applied to 0 6)
  parent: FRAME 3
  bindings:
      m : 0
      r : 6
*)

(* A.2 Recursion using ref cells *)
let factorial =
  let f = ref (fun _ -> 0) in 
    f := (fun n -> if n = 0 then 1 else n * !f (n - 1));
  !f

(* B.1 make_stat_1 *)
exception Stat_error of string

let make_stat_1 _ = 
    let sum = ref 0.0 and 
    sumsq = ref 0.0 and 
    n = ref 0 in 
    object 
      method append x = 
        begin 
          sum := x +. !sum;
          sumsq := x *. x +. !sumsq; 
          n := !n + 1
        end 
      method mean = 
        if !n = 0 then raise (Stat_error "need at least one value for mean")
         else !sum /. float_of_int !n
      method variance = 
        if !n = 0 
         then raise (Stat_error "need at least one value for variance")
          else 
           (!sumsq -. ((!sum *. !sum) /. float_of_int !n )) /. float_of_int !n
      method stdev =
        if !n = 0 
         then raise (Stat_error "need at least one value for stdev")
          else 
            sqrt(abs_float((!sumsq -. ((!sum *. !sum) /. float_of_int !n ))
             /. float_of_int !n))
      method clear = 
        begin 
          sum := 0.0;
          sumsq := 0.0;
          n := 0;
        end 
    end

(* B.2 make_stat_2 *)

(*exception Stat_error of string *)

let make_stat_2 _ = 
    let sum = ref 0.0 and 
    sumsq = ref 0.0 and 
    n = ref 0 in 
    object (self)
      method private _variance = 
        (!sumsq -. ((!sum *. !sum) /. float_of_int !n )) /. float_of_int !n
      method append x = 
        begin 
          sum := x +. !sum;
          sumsq := x *. x +. !sumsq; 
          n := !n + 1
        end 
      method mean = 
        if !n = 0 then raise (Stat_error "need at least one value for mean")
         else !sum /. float_of_int !n
      method variance = 
        if !n = 0 
         then raise (Stat_error "need at least one value for variance")
          else 
           self#_variance
      method stdev =
        if !n = 0 
         then raise (Stat_error "need at least one value for stdev")
          else 
            sqrt(abs_float(self#_variance))
      method clear = 
        begin 
          sum := 0.0;
          sumsq := 0.0;
          n := 0;
        end 
    end

(* C.1 PriorityQueue module *)
module type PRIORITY_QUEUE =
  sig
    exception Empty

    type elem      (* Abstract type of elements of queue. *)
    type t         (* Abstract type of queue. *)

    val empty      : t                (* The empty queue.         *)
    val is_empty   : t -> bool        (* Check if queue is empty. *)
    val insert     : t -> elem -> t   (* Insert item into queue.  *)
    val find_min   : t -> elem        (* Return minimum element.  *)
    val delete_min : t -> t           (* Delete minimum element.  *)
    val from_list  : elem list -> t   (* Convert list to queue.   *)
  end

module PriorityQueue : (PRIORITY_QUEUE with type elem = int) =
  struct
    exception Empty

    type elem = int

    (*
     * Data type: either
     * -- a Leaf, or
     * -- a Node of (rank, item, left heap, right heap).
     *)

    type t = Leaf | Node of int * elem * t * t

    let empty = Leaf

    let is_empty h = h = empty
    
    let rank h = 
      match h with 
        | Leaf -> 0 
        | Node(r, _, _, _) -> r

    let make_new_heap min lh rh = 
      if rank lh < rank rh 
       then Node (rank lh + 1, min, rh, lh)
        else Node (rank rh + 1, min, lh, rh)
    let rec merge_heaps h1 h2 =
      match (h1, h2) with 
        | (Leaf, _) -> h2
        | (_, Leaf) -> h1
        | (Node (_, ele1, hl1, hr1), Node (_, ele2, hl2, hr2)) -> 
            if ele1 < ele2 
             then make_new_heap ele1 hl1 (merge_heaps hr1 h2)
              else make_new_heap ele2 hl2 (merge_heaps h1 hr2) 

    let insert queue ele = merge_heaps queue (Node (1, ele, Leaf, Leaf))
    
    let find_min h = 
      match h with 
        | Leaf -> raise Empty 
        | Node (_, min, _, _) -> min
    
    let delete_min h = 
      match h with 
        | Leaf -> raise Empty 
        | Node (_, _, lh, rh) -> merge_heaps lh rh
      
    let from_list lst = 
      let rec iter result queue = 
        match queue with 
          | [] -> result 
          | h :: t -> iter (insert result h) t
      in iter empty lst
  end

let heap_sort lst = 
  let queue = PriorityQueue.from_list lst in 
  let rec iter result queue =
    if PriorityQueue.is_empty queue then result 
      else
        let min_ele = PriorityQueue.find_min queue in 
        let new_queue = PriorityQueue.delete_min queue in
        iter (min_ele :: result) new_queue
  in List.rev (iter [] queue)

(* C.2 MakePriorityQueue functor *)

(* Signature for ordered objects. *)
module type ORDERED_TYPE =
  sig
    type t
    val compare : t -> t -> int
  end

module OrderedString =
  struct
    type t = string
    let compare x y =
      if x = y then 0 else if x < y then -1 else 1
  end

module MakePriorityQueue (Elt : ORDERED_TYPE)
: (PRIORITY_QUEUE with type elem = Elt.t) =
struct

  exception Empty

  type elem = Elt.t

  type t = Leaf | Node of int * elem * t * t

  let empty = Leaf

  let is_empty h = h = Leaf

  let rank h = 
    match h with 
      | Leaf -> 0 
      | Node(r, _, _, _) -> r

  let make_new_tree min lh rh = 
    if rank lh < rank rh 
    then Node (rank lh + 1, min, rh, lh)
      else Node (rank rh + 1, min, lh, rh)
  let rec merge_tree h1 h2 =
    match (h1, h2) with 
      | (Leaf, _) -> h2
      | (_, Leaf) -> h1
      | (Node (_, ele1, hl1, hr1), Node (_, ele2, hl2, hr2)) -> 
          if Elt.compare ele1 ele2 < 0 
           then make_new_tree ele1 hl1 (merge_tree hr1 h2)
            else make_new_tree ele2 hl2 (merge_tree h1 hr2) 

  let insert queue ele = merge_tree queue (Node (1, ele, Leaf, Leaf))

  let find_min h = 
    match h with 
      | Leaf -> raise Empty 
      | Node (_, min, _, _) -> min

  let delete_min h = 
    match h with 
      | Leaf -> raise Empty 
      | Node (_, _, lh, rh) -> merge_tree lh rh
  
  let from_list lst = 
    let rec iter result queue = 
      match queue with 
        | [] -> result 
        | h :: t -> iter (insert result h) t
    in iter empty lst
end

module StringPQ = MakePriorityQueue(OrderedString)

let heap_sort_2 lst = 
  let queue = StringPQ.from_list lst in 
  let rec iter result queue =
    if StringPQ.is_empty queue then result 
      else
        let min_ele = StringPQ.find_min queue in 
        let new_queue = StringPQ.delete_min queue in
        iter (min_ele :: result) new_queue
  in List.rev (iter [] queue)

(* D.1 Streams *)

type 'a contents = Expr of (unit -> 'a) | Val of 'a
  
type 'a lazy_t = 'a contents ref

let make_lazy e = ref (Expr e)
let force lz = 
  match !lz with 
    | Val x -> x 
    | Expr e -> lz := Val (e ()); e() 

(* D.2 The Y combinator *)

let y =
  fun f ->
    (fun z -> z (`Roll z))
    (fun (`Roll w) -> f (fun x -> w (`Roll w) x))

let almost_sum = fun f -> function 
    | [] -> 0 
    | h :: t -> h + f t
let sum = y almost_sum

let factorial2 = 
  let almost_iter f =
    fun (n , r) -> if n = 0 then r else f (n - 1, n * r)
  in 
  fun n -> y almost_iter (n,1)
