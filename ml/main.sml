Control.Print.printLength := 100;
Control.Print.printDepth := 100;

(* Question 1*)
fun merge x [] = x 
  | merge [] y = y
  | merge (x::xl) (y::yl) = 
      if x < y then x::(merge xl (y::yl))
      else y::(merge (x::xl) yl)
(* merge [1,3,5,7,9] [2,4,6,8,10]; *)


(* What if we pass a empty list to the function *)
(* Question 2 *)
fun split L =
  let
    fun spl l1 l2 [] = (l1,l2)
      | spl l1 l2 (x::xl) =
          if length l1 > length l2 then spl l1 (l2@[x]) xl
          else spl (l1@[x]) l2 xl;
  in
    spl [] [] L
  end
(* split [1,4,2,6,8,3,9,5,4]; *)


(* Question 3 *)
fun mergeSort [] = [] 
  | mergeSort [x] = [x]
  | mergeSort L = 
    let 
      val (l1,l2) = split L
    in
      merge (mergeSort l1) (mergeSort l2)
    end
(* mergeSort [1,4,2,6,8,3,9,5,4]; *)

(* Question 4 *)
fun sort (op <) [] = []
  | sort (op <) [x] = [x] 
  | sort (op <) L = 
    let
      fun merge x [] = x
        | merge [] y = y
        | merge (x::xl) (y::yl) =
        if x < y then x::(merge xl (y::yl))
        else y::(merge (x::xl) yl)
      fun spl L =
        let
          fun spl l1 l2 [] = (l1,l2)
            | spl l1 l2 (x::xl) =
                if length l1 > length l2 then spl l1 (l2@[x]) xl
                else spl (l1@[x]) l2 xl;
        in
          spl [] [] L
        end
      val (l1,l2) = split L
      in
        merge (sort (op <) l1) (sort (op <) l2) 
      end

(* Question 5 *)
datatype 'a tree = empty
                 | node of 'a * 'a tree * 'a tree 
                 | leaf of 'a
(*
val tree1 = node (5, node (4, leaf 3, empty),
                     node (8, node (7, leaf 6, empty),
                              node (9, empty, leaf 10)));

*)

(* Question 6 *)
fun labels empty = []
  | labels (leaf x) = [x]
  | labels (node (d, left, right)) = (labels left) @ [d] @ (labels right)

(* Question 7 *)
infix ==
fun x == y = if x == y then true else false

fun replace (op ==) (x:'a) (y:'a) empty = empty
  | replace (op ==) x y (leaf z) =
      if x == z then leaf y else leaf z
  | replace (op ==) x y (node (z, left, right)) =
      let
        val left' = replace (op ==) x y left
        val right' = replace (op ==) x y right
      in
        if x == z then node (y, left', right')
        else node(z, left', right')
      end

(* Question 8 *)
fun replaceEmpty N empty = N
  | replaceEmpty N (leaf x) = leaf x
  | replaceEmpty N (node (d, left, right)) = node (d, (replaceEmpty N left), (replaceEmpty N right))

(* test function
fun increment empty = leaf 0
  | increment (leaf a) = leaf (a+1)
  | increment (node (a, L, R)) = node (a+1, L, R)
*)

(* Question 9 *)
fun mapTree f empty = f empty
  | mapTree f (leaf a) = f (leaf a)
  | mapTree f (node (a, L, R)) = f (node (a, (mapTree f L), (mapTree f R)))


(* Question 10 *)
fun sortTree (op <) N = 
  mapTree (fn empty => empty
            | (leaf a) => leaf (sort (op <) a)
            | (node (d',L',R')) => node ((sort (op <) d'), L', R')) N

(*
fun sortTree (op <) (leaf a) = leaf (sort (op <) a)
  | sortTree (op <) empty = empty
  | sortTree (op <) (node (d, L, R)) = node ((sort (op <) d), (sortTree (op <) L), (sortTree (op <) R))
*)
