(* A.1 Expressions *)

(* 1. 10;; *)
(* - : int = 10 *)

(* 2. 10.;; *)
(* - : float = 10. *)

(* 3. 5 + 3 + 4;; *)
(* - : int = 12 *)

(* 4. 3.2 + 4.2;; *)
(* Error: This expression has type float but an expression was expected of type
   int *)
(* Error occurred because both numbers have to be an int when using the add
   operator, but both are float *)

(* 5. 3 +. 4;; *)
(* Error: This expression has type int but an expression was expected of type 
   float *)
(* Error occurred because both when using +., it's adding two floats, but both 
   are ints *)

(* 6. 3 + 4.2;; *)
(* Error: This expression has type float but an expression was expected of type
   int *)
(* Error occurred because the second number is a float even though using + 
   operator requires both numbers to be int *)

(* 7. 3 +. 4.2;; *) 
(* Error: This expression has type int but an expression was expected of type 
   float *)
(* Error occurred because the first number is an int even though using +. 
   operator requires both numbers to be a float *)

(* 8. 3.0 +. 4.2;; *)
(* - : float = 7.2 *)

(* 9. 9 - 3 - 1;; *)
(* - : int = 5 *)

(* 10. 9 - (3 - 1);; *)
(* - : int = 7 *)

(* 11. let a = 3;; *)
(* val a : int = 3 *)

(* 12. let b = a + 1;; *)
(* val b : int = 4 *)

(* 13. a = b;; *)
(* - : bool = false *)

(* 14. [1; 2; 3] = [1; 2; 3];; *)
(* - : bool = true *)

(* 15. [1; 2; 3] ==  [1; 2; 3];; *)
(* - : bool = false *) 
(* It's different because = test to see if both sides are the same while == 
   checks to see if both sides are the same in memory *)

(* 16. [(1, 2, 3)];; *)
(* - : (int * int * int) list = [(1, 2, 3)] *) 

(* 17. [1, 2, 3] *)
(* - : (int * int * int) list = [(1, 2, 3)] *)
(* By using commas, OCaml thinks it's a 3-element int tuple in a list which 
   gives the previous result *)

(* 18. if b > a && b < a * b then b else a;; *)
(* - : int = 4 *)

(* 19. if b > a and b < a * b then b else a;; *)
(* Error: Syntax error *)
(* Error occurred because ocaml uses && to compare two conditions *)

(* 20. 2 + if b > a then b else a;; *)
(* - : int = 6 *)

(* 21. if b > a then b else a + 2;; *)
(* - : int = 4 *)
(* This is different from the previous case because we were adding 2 after the 
   if statement returns a value while in this case we are adding 2 to a if the
   condition is false *)

(* 22. (if b > a then b else a) + 2;; *)
(* - : int = 6 *)

(* 23. if b > a then b *)
(* Error: This expression has type int but an expression was expected of type 
   unit because it is in the result of a conditional with no else branch *)
(* Error occurred because since there is no else statement, then the then
   condition must have a unit type but b is an int *)

(* A.2 SICP, Problem 1.3 *)

let sum_of_squares_of_two_largest = 
  fun x1 x2 x3 -> if x1 >= x3 && x2 >= x3 then 
    x1 * x1 + x2 * x2 
  else if x2 >= x1 && x3 >= x1 then 
    x2 * x2 + x3 * x3
  else x1 * x1 + x3 * x3

(* A.3 SICP, Problem 1.4 *)

(* let a_plus_abs_b a b = 
  (if b > 0 then (+) else (-)) a b *)
(* If b is positive, the function will add a and b. If b is negative, the
   function will subtract b from a *)

(* B.1 SICP, Problem 1.5 *)

(* When Ben is using an applicative order evaluation, the test will solve for 
   p, but p is returning itself by recursion so there's no break which means it 
   is going to be in a infinite loop *)
(* When Ben is using a normal order evaluation, the test will check x first
   which means it will go through the if statement and return 0 *)

(* B.2 SICP, Problem 1.6 *)

(* When Alyssa attempts to use this to compute square roots, it's using 
   applicative order evaluation, so new_if has to solve predicate, 
   then_clause, and else_clause. However, this means it's going to call on
   sqrt_iter recursively so it will lead to an infinite loop *)

(* B.3 SICP, Problem 1.9 *)

(* 1. *)

(* add_a is recursive and add_b is iterative because add_b is proportional to 
   the amount of space it takes while add_a takes up more space to keep track 
   of when to increment *)

(* 2. *)

(*
let rec add_a a b = 
  if a = 0
    then b 
    else inc (add_a (dec a) b)

desugar this to: let rec add_a -> fun a b -> if a = 0 then b else inc (add_a (dec a) b)
bind the name "inc" to the value: fun a b -> if a = 0 then b else inc (add_a (dec a) b)

evaluate (add_a 2 5)
  evaluate 2 -> 2 
  evaluate 5 -> 5 
  evaluate add_a a b -> if a = 0 then b else inc (add_a (dec a) b)
    apply add_a a b -> if a = 0 then b else inc (add_a (dec a) b) to 2,5
      substitute 2 for a, 5 for b in fun a b -> if a = 0 then b else inc (add_a (dec a) b)
        -> if 2 = 0 then 5 else inc (add_a (dec 2) 5)
      evaluate (if 2 = 0 then 5 else inc (add_a (dec 2) 5))
        if is a special form, so evaluate the first operand
          evaluate (2 = 0)
            evaluate 2 -> 2
            evaluate 0 -> 0 
            evaluate = -> primitive function
          apply = to 2,0 -> false
        first argument of if is false, so evaluate the third operand:
          evaluate (inc (add_a (dec 2) 5))
            evaluate (dec 2)
              evaluate 2 -> 2
              evaluate dec -> primitive function 
            apply dec to 2 -> 1
          evaluate add_a a b -> if a = 0 then b else inc (add_a (dec a) b)
            apply add_a a b -> if a = 0 then b else inc (add_a (dec a) b) to 1,5
              substitute 1 for a, 5 for b in fun a b -> if a = 0 else inc (add_a (dec a) b)
                -> if 1 = 0 then 5 else inc (add_a (dec 1) 5))
              evaluate (if 1 = 0 then 5 else inc (add_a (dec 1) 5))
                if is a special form so evaluate the first operand
                  evaluate (1 = 0)
                    evaluate 1 -> 1 
                    evaluate 0 -> 0 
                    evaluate = -> primitive function 
                  apply = to 1,0 -> false 
                first argument of if is false, so evaluate the third operand
                  evaluate (inc (add_a (dec 1) 5))
                    evaluate (dec 1)
                      evaluate 1 -> 1
                      evaluate dec -> primitive function
                    apply dec to 1 -> 0 
                >>> evaluate (add_a 0 5))
                >>> evaluate 0 -> 0
                >>> evalulate 5 -> 5 
                >>> evaluate add_a -> function
                >>> apply add_a a b to 0,5 
                  >>> a = 0 and b = 5 
                    >>> if 0 = 0 then return b = 5 
                  >>> add_a (dec 1) 5 -> 5
                evaluate add_a a b -> if a = 0 then b else inc (add_a (dec a) b)
                  apply add_a a b -> if a = 0 then b else inc (add_a (dec a) b) to 0,5
                    substitute 0 for a, 5 for b in fun a b -> if a = 0 else inc (add_a (dec a) b)
                      -> if 0 = 0 then 5 else inc (add_a (dec 0) 5))
                    evaluate (if 0 = 0 then 5 else inc (add_a (dec 0) 5))
                      if is a special form so evaluate the first operand
                        evaluate (0 = 0)
                          evaluate 0 -> 0
                          evaluate 0 -> 0 
                          evaluate = -> primitive function 
                        apply = to 0,0 -> true 
                      first argument of if is true, so evaluate second operand 
                        evaluate b -> 5
                        evaluate 5 -> 5
                  evaluate inc(inc 5)
                    evaluate (inc 5)
                      evaluate 5 -> 5
                      evalulate inc -> primitive function
                      apply inc to 5 -> 6 
                    evaluate inc(6)
                      evaluate 6 -> 6 
                      evalulate inc -> primitive function 
                    apply inc to 6 -> 7 
                    result = 7
*)

(* 3. *)

(*
let rec add_b a b =
  if a = 0
     then b
     else add_b (dec a) (inc b)

Desugar this to:

let rec add_b =
  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Bind the name "add_b" to the value:

  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)
 
Evaluate (add_b 2 5)
>>> evaluate 2 -> 2 
>>> evaluate 5 -> 5
>>> evaluate add_b a b -> if a = 0 then b else add_b (dec a) (inc b)
  substitute 2 for a, 5 for b in (if ...)
    -> if 2 = 0 then 5 else add_b (dec 2) (inc 5)
  evaluate (if 2 = 0 then 5 else add_b (dec 2) (inc 5))
    if is a special form, so evaluate the first operand:
      evaluate (2 = 0)
      >>> evaluate 2 -> 2 
      >>> evaluate 0 -> 0 
      >>> evaluate = -> primitive function
        apply = to 2, 0 -> false
    first argument of if is false, so evaluate the third operand:
      evaluate (add_b (dec 2) (inc 5))
        evaluate (dec 2)
        >>> evaluate 2 -> 2 
        >>> evaluate dec -> primitive function
          apply dec to 2 -> 1 
        evaluate (inc 5)
        >>> evaluate 5 -> 5
        >>> evaluate inc -> primitive function
          apply inc to 5 -> 6
        >>> evaluate add_b a b -> if a = 0 then b else add_b (dec a) (inc b)
        apply (fun a b -> if ...) to 1, 6
        substitute 1 for a, 6 for b in (if ...)
          -> if 1 = 0 then 6 else add_b (dec 1) (inc 6) 
        evaluate (if 1 = 0 then 6 else add_b (dec 1) (inc 6)) ***
          if is a special form, so evaluate the first operand:
            evaluate (1 = 0)
            >>> evaluate 1 -> 1
            >>> evaluate 0 -> 0
            >>> evaluate = -> primitive function
              apply = to 1, 0 -> false
          first argument of if is false, so evaluate the third operand:
            evaluate (add_b (dec 1) (inc 6))
              evaluate (dec 1)
              >>> evaluate 1 -> 1
              >>> evaluate dec -> primitive function
                apply dec to 1 -> 0
              evaluate (inc 6)
              >>> evaluate 6 -> 6
              >>> evaluate inc -> primitive function
                apply inc to 6 -> 7
              >>> evaluate add_b a b -> if a = 0 then b else add_b (dec a) (inc b)
              apply (fun a b -> if ...) to 0, 7
              substitute 0 for a, 7 for b in (if ...)
                -> if 0 = 0 then 7 else add_b (dec 0) (inc 7)
              evaluate (if 0 = 0 then 7 else add_b (dec 0) (inc 7))
                if is a special form, so evaluate the first operand:
                  evaluate (0 = 0)
                  >>> evaluate 0 -> 0
                  >>> evaluate 0 -> 0
                  >>> evaluate = -> primitive function 
                    apply = to 0, 0 -> true
                first argument of if is true, so evaluate the second operand
                  >>> evaluate 7 -> 7
                  result: 7
*)

(* C.1 Computing e *)
let rec factorial n =
  if n = 0 
    then 1 
  else 
    n * factorial (n - 1)

(* a. e_term *)

let e_term n = 
  1.0 /. (float_of_int (factorial n))

(* b. e_approximation *)

let rec e_approximation n = 
  if n = 0 then 1.0 else (e_term (n))  +. (e_approximation (n - 1))

(* c. Computing the approximation *)

(* e_approximation 20;; *) 
(* - : float = 2.71828182845904553 *)
(* exp 1.0 *)
(* - : float = 2.71828182845904509 *)

(* d. Too many terms? *)

(* e_approximation 100;; *)
(* - : float = infinity *)
(* This happens because 100 factorial is too large so it becomes 0 and 1/0 is treated as infinity in ocaml *)

(* C.2 Mutual recursion *)

let rec is_even n = 
  if n = 0 then true else is_odd (n - 1)
and is_odd n =
  if n = 0 then false else is_even (n - 1)

(* C.3 SICP, Problem 1.11 *)

let rec f_rec n = 
  if n < 3 then n else f_rec(n-1) + 2 * f_rec(n-2) + 3 * f_rec(n-3)

let f_iter n = 
  let f n1 n2 n3 = n1 + 2 * n2 + 3 * n3 in 
  let rec f_fun n1 n2 n3 i  = if i <= n then f_fun (f n1 n2 n3 ) n1 n2 (i+1)  else n1 in
  if n < 3 then n else f_fun 2 1 0 3 

(* C.4 SICP, Problem 1.12 *)

let rec pascal_coefficient row idx =
  match (row,idx) with 
  | (_,1) -> 1 
  | (r, i) when r = i -> 1  
  | (r, i) when i > r -> failwith "invalid arguments "
  | (_,0) -> failwith "invalid arguments"
  | (_,_) -> pascal_coefficient (row-1) (idx-1) + pascal_coefficient (row-1) (idx)