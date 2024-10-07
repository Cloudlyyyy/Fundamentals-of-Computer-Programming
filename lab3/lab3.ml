(* A.1 SICP, Problem 2.17 *)

let rec last_sublist = function 
  | [] -> invalid_arg("last_sublist: empty list")
  | [x] -> [x]
  | h :: t -> last_sublist t

(* A.2 SICP, Problem 2.18 *)

let reverse lst = 
  let rec iter result curr_lst = 
    match curr_lst with 
      | [] -> result
      | h :: t -> iter (h::result) t
  in iter [] lst

(* A.3 SICP, Problem 2.21 *)
let rec square_list = function 
  | [] -> []
  | h :: t -> (h*h) :: square_list t 
let square_list2 items = List.map (fun x -> x * x) items

(* A.4 SICP, Problem 2.22 *)

(* Defining square_list this way produces the answer list in the reverse order
   of the one desired because when using the :: constructor, it "adds a new
   value at the front of a list" where the right side in the list and the left
   side is the element that is being added to the front of the list. So, every
   iteration makes the next number being added on the left side, so it will 
   result in having the last element in the orginial list be the first element
   in the new list and the first element in the orginal list be the last
   element in the new list which is the reverse order. *)


(* This doesn't work either because there's a type error. The left side of the 
   :: is expecting a type 'a but answer is 'a list while the right side is 
   expecting 'a list but (h*h) is an int. *)

let square_list items =
  let rec iter things answer =
    match things with
      | [] -> answer
      | h :: t -> iter t (answer @ [h*h])
  in iter items []


(* A.5 count_negative_numbers *)

let count_negative_numbers lst = 
  let rec iter lst count = 
    match lst with 
      | [] -> count
      | h :: t -> if h < 0 then iter t (count + 1) else iter t count
  in iter lst 0

(* A.6 power_of_two_list *)

let power_of_two_list n = 
  let rec iter result n = 
    let rec pow x = 
      match x with 
        | 0 -> 1
        | 1 -> 1
        | _ -> 2 * pow (x-1)
      in
      match n with 
        | 0 -> result
        | n -> iter ((pow n) :: result) (n-1)
  in iter [] n 

(* A.7 prefix_sum *)

let prefix_sum lst  = 
  let rec iter result prev curr_lst = 
      match curr_lst with 
        | [] -> List.rev result
        | h :: t -> iter ((prev + h) :: result) (prev+h) t
  in iter [] 0 lst

(* A.8 SICP, Problem 2.27 *)

let deep_reverse lst = 
  let rec iter result curr_lst = 
    match curr_lst with 
      | [] -> result
      | h :: t -> iter (reverse (h)::result) t
  in iter [] lst

(* A.9 S-expressions *)

type 'a nested_list =
  | Value of 'a
  | List of 'a nested_list list

let deep_reverse_nested nested_lst = 
  let rec iter result curr_lst = 
    match curr_lst with 
    | [] -> result 
    | Value v :: t  -> iter (Value v :: result) t
    | List l :: t -> iter ( List (iter [] l) :: result) t
  in 
    match nested_lst with 
    | Value v  -> Value v
    | List l  -> List (iter [] l)

(* B.1 Quicksort *) 

let rec quicksort cmp lst = 
    match lst with 
      | [] -> []
      | h :: t -> let list1 = List.filter (fun x -> cmp x h) t in 
                  let list2 = List.filter (fun x ->  not(cmp x h)) t in
                  (quicksort cmp list1) @ [h] @ (quicksort cmp list2)

(* B.2 Quicksort's recursion class **)

(* Quicksort function is an instance of generative recursion and not structual
   recursion because the definition of generative recursion is recursing over
   new data that was generated. In quicksort, we use filter to create a new 
   list and recur on the new list which is new data since it's taking up more
   memory, so it's a generative recursion. It is not a structural recursion 
   because we are not recursing over the original list since structural 
   recursion uses a part of the original data. *)

(* B.3 Merge sort base cases *)

(* This doesn't work because Ben Bitfiddle doesn't account for when the list 
   has only one element. In merge sort, you have to split the list into two, 
   but since there's a chance there's only one element in a list, and there's
   no case that catches that, so it will result in an infinite recursion loop *)

(* B.4 Insertion sort: recursive process version *)

(* This is an example of structural recursion because we are recursing on the 
   same list, so it's still a part of the original data which means it is a 
   structural recursion *)

let rec insert_in_order cmp new_result a_list =
  match a_list with
    | [] -> [new_result]
    | h :: t when cmp new_result h -> new_result :: a_list
    | h :: t ->  h :: insert_in_order cmp new_result t

let rec insertion_sort cmp a_list =
  match a_list with
    | [] -> []
    | h :: t -> insert_in_order cmp h (insertion_sort cmp t)

(* C.1 SICP, Problem 2.32 *)

let rec subsets = function
  | [] -> [[]]
  | h :: t -> let rest = subsets t in
      rest @ (List.map (fun x -> h :: x) rest)

(* C.2 SICP, Problem 2.33 *)

let rec accumulate op initial sequence =
  match sequence with
    | [] -> initial
    | h :: t -> op h (accumulate op initial t)

let map p sequence =
  accumulate (fun x r -> p x :: r) [] sequence

let append seq1 seq2 =
  accumulate (fun x r -> x :: r) seq2 seq1

let length sequence =
  accumulate (fun x r -> r + 1) 0 sequence

(* C.3 SICP, Problem 2.36 *)

let rec accumulate_n op init seqs =
  match seqs with
    | [] -> failwith "empty list"
    | [] :: _ -> []   (* assume all sequences are empty *)
    | h :: t -> accumulate op init (List.map List.hd seqs) :: accumulate_n op init (List.map List.tl seqs)

  
(* C.4 SICP, Problem 2.37 *)

let rec map2 f x y =
  match (x, y) with
    | ([], []) -> []
    | ([], _) -> failwith "unequal lists"
    | (_, []) -> failwith "unequal lists"
    | (_,_) -> f (List.hd x) (List.hd y) :: (map2 f (List.tl x) (List.tl y))

let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)

let matrix_times_vector m v = map (fun x -> dot_product v x) m

let transpose mat = accumulate_n (fun x y -> x :: y) [] mat

let matrix_times_matrix m n =
  let cols = transpose n in
     map (fun x -> matrix_times_vector cols x) m
