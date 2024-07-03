(* #h {f=3, g=12}
fun sum_list xs = 
  case xs of
    [] => 0
    | x::xs' => x + sum_list xs'

val test = sum_list ([1, 3, 2]) = 6;

val NONE = SOME 2; *)

(* fun f (a::b::c) = 2 + f c

| f [] = 0;
| f [] = 0;

val x = f [1,2,3];
val x = f [1,2,3]; *)

(* val x = List.Empty;

val res = (hd [], 0) handle List.Empty => raise x;  *)

(* val e = {bar=(1+2, true andalso true), foo = 3+4, baz = (false,9)};
#foo e;
val f = {1=1+2, 2=2+3};
#1 f; *)
(* 
datatype exp = Constant of int
             | Negate of exp
             | Add of exp * exp
             | Multiply of exp * exp

fun eval e =
    case e of
        Constant i => i
       | Negate e2 => ~ (eval e2)
       | Add(e1,e2) => (eval e1) + (eval e2)
       | Multiply(e1,e2) => (eval e1) * (eval e2)

val result = eval (Add (Constant 19, Negate (Constant 4)));

fun number_of_adds e =
   case e of
   Constant i => 0
   | Negate e2 => number_of_adds e2
   | Add(e1,e2) => 1 + number_of_adds e1 + number_of_adds e2
   | Multiply(e1,e2) => number_of_adds e1 + number_of_adds e2 *)

(* type foo = int;

type card = suit * rank;

type name_record = { student_num : int option,
first : string,
middle : string option,
last : string }; *)

(* datatype my_int_list = Empty
                     | Cons of int * my_int_list;
val one_two_three = Cons(1,Cons(2,Cons(3,Empty)));
fun append_mylist (xs,ys) =
    case xs of
        Empty => ys
      | Cons(x,xs') => Cons(x, append_mylist(xs',ys))

fun inc_or_zero intoption =
    case intoption of
        NONE => 0
      | SOME i => i+1

fun sum_list xs =
    case xs of
        [] => 0
      | x::xs' => x + sum_list xs'

fun append (xs,ys) =
    case xs of
        [] => ys
       | x::xs' => x :: append(xs',ys)

fun rotate_left (x,y,z) = (y,z,x)
fun rotate_right triple = rotate_left(rotate_left triple)

val triple_val = rotate_right (1, 2, 3);

fun partial_sum (x,y,z) = x + z
fun partial_name {first=x, middle=y, last=z} = x ^ " " ^ z

val partial_val = partial_name {first="hello", middle=1, last="world!"};

fun same_thing(x,y) = if x=y then "yes" else "no" (* has type ’’a * ’’a -> string *)
fun is_three x = if x=3 then "yes" else "no" (* has type int -> string *)

val same_thing_result = same_thing(1, 1);
val same_thing_result_1 = same_thing(1, 2);

fun len xs =
    case xs of
       [] => 0
      | x::xs' => 1 + len xs'
val len_val = len [1, 2, 3];

fun len xs =
    case xs of
       [] => 0
      | _::xs' => 1 + len xs'
val len_val_1 = len [1, 2, 3, 4]; *)

exception BadTriple
fun zip3 list_triple =
    case list_triple of
       ([],[],[]) => []
      | (hd1::tl1,hd2::tl2,hd3::tl3) => (hd1,hd2,hd3)::zip3(tl1,tl2,tl3)
      | _ => raise BadTriple

fun unzip3 lst =
    case lst of
       [] => ([],[],[])
     | (a,b,c)::tl => let val (l1,l2,l3) = unzip3 tl
                      in
                        (a::l1,b::l2,c::l3)
                      end

fun nondecreasing intlist =
    case intlist of
       [] => true
      | _::[] => true
      | head::(neck::rest) => (head <= neck andalso nondecreasing (neck::rest))

datatype sgn = P | N | Z
fun multsign (x1,x2) =
  let fun sign x = if x=0 then Z else if x>0 then P else N
  in
      case (sign x1,sign x2) of
          (Z,_) => Z
        | (_,Z) => Z
        | (P,P) => P
        | (N,N) => P
        | _ => N (* many say bad style; I am okay with it *)
  end

datatype exp = Constant of int | Negate of exp | Add of exp * exp | Multiply of exp * exp
(* fun eval (Constant i) = i
  | eval (Negate e2) = ~ (eval e2)
  | eval (Add(e1,e2)) = (eval e1) + (eval e2)
  | eval (Multiply(e1,e2)) = (eval e1) * (eval e2) *)

(* val eval_result = eval(Constant 1);
val eval_result_1 = eval(Negate (Constant 1));
val eval_result_2 = eval(Add (Constant 1, Negate (Constant 1)));
val eval_result_3 = eval(Multiply (Constant 2, Constant 3)); *)

(* fun append ([],ys) = ys
  | append (x::xs',ys) = x :: append(xs',ys)

val append_result = append([4], [1, 2, 3]); *)

fun eval e =
    case e of
       Constant i => i
      | Negate e2 => ~ (eval e2)
      | Add(e1,e2) => (eval e1) + (eval e2)
      | Multiply(e1,e2) => (eval e1) * (eval e2)

val eval_result = eval(Constant 1);
val eval_result_1 = eval(Negate (Constant 1));
val eval_result_2 = eval(Add (Constant 1, Negate (Constant 1)));
val eval_result_3 = eval(Multiply (Constant 2, Constant 3));

fun append e =
    case e of
       ([],ys) => ys
      | (x::xs',ys) => x :: append(xs',ys)

val append_result = append([4], [1, 2, 3]);

fun maxlist (xs,ex) =
    case xs of
        [] => raise ex
      | x::[] => x
      | x::xs' => Int.max(x,maxlist(xs',ex))
val maxlist_result = maxlist([3,4,0],List.Empty);
(* val maxlist_result_1 = maxlist([],List.Empty); *)

fun sum1 xs =
    case xs of
       [] => 0
      | i::xs' => i + sum1 xs'

fun fact1 n = if n=0 then 1 else n * fact1(n-1)

val fact1_result = fact1 3;

fun fact2 n =
    let fun aux(n,acc) = if n=0 then acc else aux(n-1,acc*n)
    in
        aux(n,1)
    end
val fact2_result = fact2 3;

val equality = fact1_result = fact2_result

fun rev1 lst =
    case lst of
       [] => []
      | x::xs => (rev1 xs) @ [x]

val rev1_result = rev1 [1, 2, 3, 4];

fun rev2 lst =
    let fun aux(lst,acc) =
            case lst of
               [] => acc
              | x::xs => aux(xs, x::acc)
    in
        aux(lst,[])
    end
val rev2_result = rev2 [1, 2, 3, 4];

val int_list = [1, 2, 3, 4];
val int_list_1 = 1::int_list;









