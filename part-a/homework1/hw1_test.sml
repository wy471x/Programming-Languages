(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

(* is_older function *)
fun is_older (first_date: int*int*int, second_date: int*int*int) =
  let
    val accu_months = [31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365]
    fun date_to_days (accu_months: int list, date: int*int*int) = 
      List.nth(accu_months, (#2 date) - 1) + (# 3 date) + (#1 date) * 365
  in
    date_to_days(accu_months, first_date) < date_to_days(accu_months, second_date)
  end

(* number_in_month function *)
fun number_in_month (dates: (int * int * int) list, month: int) =
    let
      val first_date = if null dates then (0, 0, 0) else hd(dates)
    in
      if null dates then 0
      else if (#2 first_date) = month then 1 + number_in_month(tl(dates), month)
      else number_in_month(tl(dates), month)
    end

(* number_in_months *)
fun number_in_months (dates: (int * int * int) list, months: int list) = 
  let
      val first_date = if null dates then (0, 0, 0) else hd(dates)
    in
      if null dates then 0
      else if List.exists (fn (month) => month = (#2 first_date)) months then 1 + number_in_months(tl(dates), months)
      else number_in_months(tl(dates), months)
    end

(* dates_in_month *)
fun dates_in_month (dates: (int*int*int) list, month: int) =
  let
    val first_date = if null dates then (0, 0, 0) else hd(dates)
  in
    if null dates then []
    else if (#2 first_date) = month then first_date::dates_in_month(tl(dates), month)
    else dates_in_month(tl(dates), month)
  end

(* dates_in_months *)
fun dates_in_months (dates: (int*int*int) list, months: int list) = 
  let
    val first_date = if null dates then (0, 0, 0) else hd(dates)
  in
    if null dates then []
    else if List.exists (fn (month) => month = (#2 first_date)) months then first_date::dates_in_months(tl(dates), months)
    else dates_in_months(tl(dates), months)
  end

(* get_nth *)
fun get_nth (strs: string list, n: int) = 
  let
    fun start_from(index: int, head: string, strs: string list) = 
      if index = n then head
      else start_from(index + 1, hd(strs), tl(strs))
  in
    start_from(1, hd(strs), tl(strs))
  end

(* date_to_string *)
fun date_to_string (date: int*int*int) = 
  let
    val month_list = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"] 
  in
    get_nth(month_list, (#2 date)) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date)
  end

(* number_before_reaching_sum *)
fun number_before_reaching_sum (sum: int, nums: int list) = 
  let
    fun sum_first_nth (accu_sum: int, nums: int list, n: int) = 
      if accu_sum >= sum then n
      else sum_first_nth(accu_sum + hd(nums), tl(nums), n + 1)
  in
    sum_first_nth (hd(nums), tl(nums), 0)
  end

(* what_month *)
fun what_month (day: int) = 
  let
    val accu_month = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
    number_before_reaching_sum (day, accu_month) + 1
  end

(* month_range *)
fun month_range (day1: int, day2: int) = 
  let
    fun month_list (start_day: int, end_day: int) = 
      if start_day > end_day then []
      else what_month(start_day)::month_list(start_day + 1, end_day)
  in
    month_list(day1, day2)
  end

(* oldest *)
fun oldest (dates: (int*int*int) list) = 
  let
    fun compare (pre: int*int*int, curr_dates: (int*int*int) list) = 
      if null curr_dates then SOME (pre)
      else if is_older(pre, hd(curr_dates)) then compare (pre, tl(curr_dates))
      else compare (hd(curr_dates), tl(curr_dates))
  in
    if null dates then NONE
    else compare (hd(dates), tl(dates))
  end 

val test1 = is_older ((1,2,3),(2,3,4)) = true

val test1_1= is_older ((1, 2, 25), (1, 12, 29)) = true

val test2 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1

val test3 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3

val test4 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test6_1 = get_nth (["hello", "world", "!", "!", "!"], 4) = "!"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test8_1 = number_before_reaching_sum (10, [10,2,3,4,5]) = 0

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

val test11_1 = oldest([]) = NONE