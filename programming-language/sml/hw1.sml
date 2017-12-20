fun is_older (x: int * int * int, y: int * int *int) = 
  if #1 x < #1 y
  then true
  else if #1 x > #1 y
  then false
  else if #2 x < #2 y
  then true
  else if #2 x > #2 y
  then false
  else if #3 x < #3 y
  then true
  else false
;;

fun number_in_month (dates: (int * int * int) list, month: int) =
  if null dates
  then 0
  else if #2 (hd dates) = month
  then 1 + number_in_month (tl dates, month)
  else 0 + number_in_month (tl dates, month)
;;

fun number_in_months (dates: (int * int * int) list, months: int list) = 
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)
;;

fun dates_in_month (dates: (int * int * int) list, month: int) = 
  if null dates
  then []
  else if #2 (hd dates) = month
  then hd dates::dates_in_month(tl dates, month)
  else dates_in_month(tl dates, month)
;;

fun dates_in_months (dates: (int * int * int) list, months: int list) = 
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)
;;

fun get_nth(str: string list, idx: int) = 
  if idx = 1
  then hd str
  else get_nth(tl str, idx - 1)
;;

fun date_to_string(date: int * int * int) = 
  let
    val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
    get_nth(months, #2 date) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString (#1 date)
  end
;;

fun number_before_reaching_sum(sum: int, numbers: int list) =       
  if sum <= hd numbers
  then 0
  else 1 + number_before_reaching_sum(sum - hd numbers, tl numbers)
  
fun what_month(day_of_year: int) =
  let
    val months_interval = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30 ,31]
  in 
    1 + number_before_reaching_sum(day_of_year, months_interval)
  end

fun month_range (day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1)::month_range(day1 + 1, day2)
;;

fun oldest(dates: (int * int * int) list) =
  if null dates
  then NONE
  else
    let val tl_ans = oldest(tl dates)
    in if isSome tl_ans andalso is_older(valOf tl_ans, hd dates)
    then tl_ans
    else SOME(hd dates)
    end
