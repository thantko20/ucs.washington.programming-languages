fun is_older(date1 : int*int*int, date2 : int*int*int) =
  if #3 date1 < #3 date2 then
    true
  else if #3 date1 > #3 date2 then
    false
  else if #2 date1 < #2 date2 then
    true
  else if #2 date1 > #2 date2 then
    false
  else
    #1 date1 < #1 date2

fun number_in_month(dates : (int*int*int) list, month : int) =
  if null dates
  then 0
  else let val adder = if (#2 (hd dates)) = month then 1 else 0 in adder + number_in_month(tl dates, month) end
    
fun number_in_months(dates : (int*int*int) list, months : int list) = 
  if null months
  then 0
  else number_in_month(dates, hd months) + number_in_months(dates, tl months)

fun dates_in_month(dates : (int*int*int) list, month : int) =
  if null dates
  then []
  else
    let
      val date_month = #2  (hd dates)
      val tl_ans_dates = dates_in_month(tl dates, month)
    in 
      if date_month = month
      then hd dates::tl_ans_dates
      else tl_ans_dates
    end
  
fun dates_in_months(dates : (int*int*int) list, months : int list) =
  if null months
  then []
  else dates_in_month(dates, hd months) @ dates_in_months(dates, tl months)

fun get_nth(strings : string list, n : int) =
  if n < 1
  then NONE
  else
    let
      fun get_nth_nonempty(strings: string list, n : int) =
        if n = 1
        then hd strings 
        else get_nth_nonempty(tl strings, n - 1)
    in
      SOME(get_nth_nonempty(strings, n))
    end

val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

fun date_to_string(date : (int*int*int)) =
  valOf(get_nth(months, #2 date)) ^ "-" ^ Int.toString(#1 date) ^ "-" ^ Int.toString(#3 date)

fun number_before_reaching_sum(sum : int, numbers : int list) =
  if null numbers orelse hd numbers >= sum
  then 0
  else number_before_reaching_sum(sum - hd numbers, tl numbers) + 1

val month_lengths = [31,28,31,30,31,30,31,31,30,31,30,31]

fun what_month(day : int) =
  number_before_reaching_sum(day, month_lengths) + 1

fun month_range(day1: int, day2: int) =
  if day1 > day2
  then []
  else what_month(day1)::month_range(day1 + 1, day2)
  
fun oldest(dates: (int*int*int) list) =
  if null dates
  then NONE
  else
    let
      fun oldest_nonempty(dates: (int*int*int) list) =
        if null (tl dates)
        then hd dates
        else
          let
            val tl_oldest = oldest_nonempty(tl dates)
          in
            if is_older(hd dates, tl_oldest) then hd dates else tl_oldest
          end
    in
      SOME(oldest_nonempty(dates))
    end

fun cumulative_sum(numbers: int list) =
  let
    fun loop(track: int, l: int list) =
      if null l
      then []
      else
        let
          val result = hd l + track
        in
          result::loop(result, tl l)
        end
  in
    loop(0, numbers)
  end

fun remove_duplicates(items: int list) =
  if null items
  then []
  else
    let
      val tl_items = remove_duplicates(tl items)
      val hd_item = hd items
    in
      if null(tl_items)
      then hd_item::tl_items
      else if hd_item = hd tl_items
      then tl_items
      else hd_item::tl_items
    end

fun number_in_months_challenge(dates : (int*int*int) list, months : int list) =
  let
    val unique_months = remove_duplicates(months)
  in
    number_in_months(dates, unique_months)
  end

fun dates_in_months_challenge(dates : (int*int*int) list, months : int list) =
  let
    val unique_months = remove_duplicates(months)
  in
    dates_in_months(dates, unique_months)
  end