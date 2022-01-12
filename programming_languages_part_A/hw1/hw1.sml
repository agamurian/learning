(*
You will write 11 SML functions (and tests for them) related to calendar dates. In all problems, a “date”
is an SML value of type int*int*int, where the first part is the year, the second part is the month, and
the third part is the day. A “reasonable” date has a positive year, a month between 1 and 12, and a day no
greater than 31 (or less depending on the month). Your solutions need to work correctly only for reasonable
dates, but do not check for reasonable dates (that is a challenge problem) and many of your functions will
naturally work correctly for some/all non-reasonable dates. A “day of year” is a number from 1 to 365
where, for example, 33 represents February 2. (We ignore leap years except in one challenge problem.)
*)


(* Write a function is_older that takes two dates and evaluates to true or false. It evaluates to true if
the first argument is a date that comes before the second argument. (If the two dates are the same,
the result is false.) *)
fun is_older (date_1 : int*int*int, date_2 : int*int*int) =
    if (#1 date_1 > #1 date_2)
    then false
    else if (#1 date_1 < #1 date_2)
    then true
    else if (#2 date_1 > #2 date_2)
    then false
    else if (#2 date_1 < #2 date_2)
    then true
    else if (#3 date_1 > #3 date_2)
    then false
    else if (#3 date_1 < #3 date_2)
    then true
    else false


(* 2. Write a function number_in_month that takes a list of dates and a month (i.e., an int) and returns
how many dates in the list are in the given month. *)
fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else if (#2 (hd dates))=month
    (* there is no exponitial blowup because algorithm goes
     then OR else, only one of them elvas,
     so algo goes one OR the other branch everytime*)
        then 1 + number_in_month((tl dates),month)
        else 0 + number_in_month((tl dates),month)


(* 3. Write a function number_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns the number of dates in the list of dates that are in any of the months in the list of months.
Assume the list of months has no number repeated. Hint: Use your answer to the previous problem. *)
fun number_in_months (dates: (int*int*int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))


(* 4. Write a function dates_in_month that takes a list of dates and a month (i.e., an int) and returns a
list holding the dates from the argument list of dates that are in the month. The returned list should
contain dates in the order they were originally given. *)
fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else if (#2 (hd dates)) = month
        then (hd dates) :: dates_in_month((tl dates), month)
        else dates_in_month((tl dates), month)


(* 5. Write a function dates_in_months that takes a list of dates and a list of months (i.e., an int list)
and returns a list holding the dates from the argument list of dates that are in any of the months in
the list of months. Assume the list of months has no number repeated. Hint: Use your answer to the
previous problem and SML’s list-append operator (@). *)
fun dates_in_months (dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))


(* 6. Write a function get_nth that takes a list of strings and an int n and returns the n th element of the
list where the head of the list is 1 st . Do not worry about the case where the list has too few elements:
your function may apply hd or tl to the empty list in this case, which is okay.*)
fun get_nth (strs: string list, n: int) =
    if (n = 1)
    then (hd strs)
    else (get_nth((tl strs), n - 1))


(* 7. Write a function date_to_string that takes a date and returns a string of the form January 20, 2013
(for example). Use the operator ^ for concatenating strings and the library function Int.toString
for converting an int to a string. For producing the month part, do not use a bunch of conditionals.
Instead, use a list holding 12 strings and your answer to the previous problem. For consistency, put a
comma following the day and use capitalized English month names: January, February, March, April,
May, June, July, August, September, October, November, December. *)
fun date_to_string (date : int*int*int) =
    let
        val months_names = ["January", "February", "March", "April","May", "June", "July", "August", "September","October", "November", "December"]

        fun get_day (day:int) =
            Int.toString(day)

        fun get_month (month:int) =
            get_nth(months_names,month)

        fun get_year (year:int) =
            Int.toString(year)

    in
         get_month(#2 date) ^ " " ^ get_day(#3 date) ^ ", " ^ get_year(#1 date)
    end

(* 8. Write a function number_before_reaching_sum
that takes
an int called sum,
    which you can assume is positive,
and an int list,
    which you can assume contains all positive numbers,
and returns an int.

You should return an int n such that the first n elements of the list add to less than sum, but the first
n + 1 elements of the list add to sum or more. Assume the entire list sums to more than the passed in
value; it is okay for an exception to occur if this is not the case. *)

fun nbrs (sum: int, xs: int list) =
    let
        fun sum_list(xs: int list) =
            if null xs
            then 0
            else hd xs + sum_list(tl ys)
    in
        if null xs
        then 0
        else if sum < sum_list(xs)
                    then 0 + nbrs(sum,(tl xs))
                    else 1 + nbrs(sum,(tl xs))
    end
