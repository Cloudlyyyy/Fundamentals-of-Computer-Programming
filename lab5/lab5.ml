(* A.1 Fibonacci again *)

let fibonacci n = 
  let prev_1 = ref 0 
  and prev_2 = ref 1 
  and curr = ref 0 
  and counter = ref 0 in 
  if n < 2 
    then n else 
      begin
        while !counter < (n-1) do 
          curr := !prev_1 + !prev_2;
          prev_1 := !prev_2;
          prev_2 := !curr; 
          counter := !counter + 1
        done;
        !curr
      end

let fibonacci2 n = 
  let prev_1 = ref 0 
  and prev_2 = ref 1 
  and curr = ref 0 in
  if n < 2 then n else 
    begin 
      for _ = 2 to n do 
        curr := !prev_1 + !prev_2; 
        prev_1 := !prev_2;
        prev_2 := !curr;
      done;
      !curr
    end

(* A.2 Bubble sort *)

let bubble_sort arr = 
  let x = ref (Array.length arr - 1) in 
  let swapped = ref true in 
  while (!swapped) = true do 
    swapped := false;
    for i = 1 to !x do 
      if arr.(i) < arr.(i-1) then
        let temp = arr.(i-1) in 
        arr.(i-1) <- arr.(i);
        arr.(i) <- temp;
        swapped := true
    done;
    x := !x - 1
  done

(* B.1 Extending get_meter *)

let meters_per_foot = 0.3048

let get_meters len =
  match len with
    | `Meter m -> m
    | `Foot f -> f *. meters_per_foot
    | `Inch i -> i  *. meters_per_foot /. 12.

let length_add a b = `Meter (get_meters a +. get_meters b)

let grams_per_slug = 14593.903203

(* B.2 Mass and time abstractions *)
let get_grams len = 
  match len with 
    | `Gram g -> g 
    | `Kilo k -> k *. 1000.
    | `Slug s -> s *. 14593.903203 

let mass_add a b = `Gram (get_grams a +. get_grams b)

let get_seconds time = 
  match time with 
    | `Second s -> s 
    | `Minute m -> m *. 60.
    | `Hour h -> h *. 60. *. 60. 
    | `Day d -> d *. 24. *. 60. *. 60.

let time_add a b = `Second (get_seconds a +. get_seconds b)

(* B.3 Adding units *)

let unit_add a b = 
  match a,b with 
    | `Length a, `Length b -> `Length(length_add a b)
    | `Mass a , `Mass b -> `Mass (mass_add a b )
    | `Time a, `Time b -> `Time (time_add a b )
    | _,_ -> failwith "not compatible"

(* There is no combinatorial explosion when adding more unit classes, because 
   adding a new case doesn't necessarily increase the complexity because when 
   adding a case, we are doing arithmetic to get the type we want, so the 
   complexity does not increase so  therefore, there is no combinatorial 
   explosion *)


(* C.1 Object-oriented grams *)

let rec make_gram g =
  let comp other = 
    match other with 
    | `Gram -> true 
    | `Slug -> true
    | _ -> false
  in
    object
      method get_grams = g
      method get_slugs = 14593.903203 *. g 
      method unit_type = `Gram
      method compatible other = comp other#unit_type
      method add other = if comp other#unit_type
                          then make_gram (g +. other#get_grams)
                            else failwith "incompatible units"
    end

(* C.2 Object-oriented differentiator *)

(* Define a number as a message-passing object. *)
(* "i" is an int. *)
let rec make_number i =
  object
    method value = i
    method show = string_of_int i
    method is_zero = i = 0
    method is_number = true
    method evaluate _ _ = make_number i  (* must evaluate to an object *)
    method derive _ = make_number 0  (* derivative of a number is 0 *)
  end

(* Define a variable as a message-passing object. *)
(* "v" is a string. *)
let rec make_variable v =
  object
    method value = failwith "variable has no numerical value"
    method show  = v
    method is_zero = false
    method is_number = false
    method evaluate v' n =
      if v = v'
        then make_number n
        else make_variable v
    method derive v' =
      if v = v'
        then make_number 1  (* d/dx(x) = 1 *)
        else make_number 0  (* d/dx(y) = 0 *)
  end

(* Define a sum as a message-passing object. *)
let rec make_sum expr1 expr2 =
  match () with
    | _ when expr1#is_zero -> expr2  (* 0 + expr = expr *)
    | _ when expr2#is_zero -> expr1  (* expr + 0 = expr *)
    | _ when expr1#is_number && expr2#is_number ->  (* add numbers *)
          make_number (expr1#value + expr2#value)
    | _ ->  (* create a new object representing the sum *)
          object
            method value = failwith "sum expression has no numerical value"
            method show = "(" ^ expr1#show ^ " + " ^ expr2#show ^ ")"
            method is_zero = false
            method is_number = false
            method evaluate v n =
              make_sum (expr1#evaluate v n) (expr2#evaluate v n)
            method derive v =
              make_sum (expr1#derive v) (expr2#derive v)
          end

(* Evaluate a message-passing expression with a number
   substituted for a variable. *)
let evaluate expr v n = expr#evaluate v n

(* Return the string representation of an expression. *)
let show expr = expr#show

(* Return the derivative of an expression. *)
let differentiate expr v = expr#derive v

(* a. make_product *)
let rec make_product expr1 expr2 = 
  match () with 
  | _ when expr1#is_zero -> make_number 0 
  | _ when expr2#is_zero -> make_number 0 
  | _ when expr1#is_number && expr1#value = 1 -> expr2
  | _ when expr2#is_number && expr2#value = 1 -> expr1
  | _ when expr1#is_number && expr2#is_number -> make_number 
      (expr1#value * expr2#value)
  | _ -> 
        object 
          method value = failwith "sum expression has no numerical value"
          method show = "(" ^ expr1#show ^ " + " ^ expr2#show ^ ")"
          method is_zero = false
          method is_number = false
          method evaluate v n =
            make_sum (expr1#evaluate v n) (expr2#evaluate v n)
          method derive v =
            make_sum (make_product (expr1#derive v) expr2)
            (make_product (expr2#derive v) expr1)  
        end 

(* testing the differentiator *)

(* i.*)

(*
val f :
  < derive : string -> 'a; evaluate : string -> int -> 'a;
    is_number : bool; is_zero : bool; show : string;
    value : int >
  as 'a = <obj> 
*)

(* ii. *)

(*
val dfdx :
  < derive : string -> 'a; evaluate : string -> int -> 'a;
    is_number : bool; is_zero : bool; show : string;
    value : int >
  as 'a = <obj> 
*)

(* iii. *)

(* 
- : string =
"(((x + (x + y)) + (((x + y) + (y + x)) + x)) + (((x + (y + y)) + ((y + y) + x)) + 3))"
*)

(* iv. *)

(*
- : string =
"((3 + (3 + (3 + y))) + ((3 + (3 + (3 + (y + y)))) + ((y + y) + 2)))"
*)

(* v. *)

(* - : string = "40" *)

(* vi. *)

(* - : string = "52" *)


