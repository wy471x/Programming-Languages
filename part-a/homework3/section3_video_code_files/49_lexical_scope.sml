(* Programming Languages, Dan Grossman *)
(* Section 3: Lexical Scope *)

(* 1 *) val x = 1

(* 2 *) fun f y = x + y

(* 3 *) val x = 2

(* 4 *) val y = 3

(* 5 *) val z = f (x + y)

fun f g = let val x = 9 in g() end
val x = 7
fun h() = x+1
val y = f h