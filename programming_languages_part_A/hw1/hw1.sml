fun is_older (date_1 : int*int*int, date_2 : int*int*int) =
    if (#1 date_1 < #1 date_2)
    then true
    else if (#1 date_1 > #1 date_2)
    then false
    else if (#2 date_1 < #2 date_2)
    then true
    else if (#2 date_1 > #2 date_2)
    then false
    else if (#3 date_1 < #3 date_2)
    then true
    else false


fun number_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then 0
    else if (#2 (hd dates))=month
    (* there is no exponitial blowup because algorithm goes
     then OR else, only one of them elvas,
     so algo goes one OR the other branch everytime*)
        then 1 + number_in_month((tl dates),month)
        else 0 + number_in_month((tl dates),month)


fun number_in_months (dates: (int*int*int) list, months: int list) =
    if null months
    then 0
    else number_in_month(dates, (hd months)) + number_in_months(dates, (tl months))


fun dates_in_month (dates : (int*int*int) list, month : int) =
    if null dates
    then []
    else if (#2 (hd dates)) = month
        then (hd dates) :: dates_in_month((tl dates), month)
        else dates_in_month((tl dates), month)


fun dates_in_months (dates: (int*int*int) list, months: int list) =
    if null months
    then []
    else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))


fun get_nth (strs: string list, n: int) =
    if (n = 1)
    then (hd strs)
    else (get_nth((tl strs), n - 1))


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

(*was stuck here for 3 days, did it only with a hint in forums*)
fun number_before_reaching_sum (sum: int, xs: int list) =
    if null xs
    then 0
    else
        if sum <= hd (xs)
        then 0
        else 1 + number_before_reaching_sum(sum - (hd xs), (tl xs))


fun what_month (day: int) =
    let val months_list = [31,28,31,30,31,30,31,31,30,31,30] in
        number_before_reaching_sum(day, months_list) + 1
    end


fun month_range (day1, day2) =
    if day1 > day2
    then []
    else what_month(day1) :: month_range((day1 + 1),day2)


fun oldest (dates : (int*int*int) list) =
    if null dates
    then NONE
    else let (* fine to assume argument nonempty because it is local *)
        fun oldest_nonempty (xs : (int*int*int) list) =
            if null (tl xs) (* xs better not be [] *)
            then hd xs
            else let val tl_ans = oldest_nonempty(tl xs)
                 in
                     if is_older(hd xs, tl_ans)(* hd xs < tl_ans *)
                     then hd xs
                     else tl_ans
                 end
    in
        SOME (oldest_nonempty dates)
    end
