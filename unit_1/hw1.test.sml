use "hw1.sml";

val is_older_1_res = is_older((1,2,2025), (1,2,2024)) = false
val is_older_2_res = is_older((1,2,2023), (1,2,2024)) = true
val is_older_3_res = is_older((15, 3, 2024), (11, 2, 2024)) = false
val is_older_4_res = is_older((15, 1, 2024), (11, 2, 2024)) = true
val is_older_5_res = is_older((13, 1, 2024), (11, 1, 2024)) = false

val number_in_month_1_res = number_in_month([(1,2,2025), (12,2,2025), (15,6,2025)], 2) = 2
val number_in_month_2_res = number_in_month([(1,2,2025), (12,2,2025), (15,6,2025)], 9) = 0

val number_in_months_1_res = number_in_months([(1,2,2025), (12,2,2025), (15,6,2025)], [1,3,5]) = 0
val number_in_months_2_res = number_in_months([(1,2,2025), (12,2,2025), (15,6,2025)], [1,3,2]) = 2
val number_in_months_3_res = number_in_months([(1,2,2025), (12,2,2025), (15,6,2025)], [1,2,6]) = 3

val dates_in_month_1_res = dates_in_month([(1,1,2025)], 2) = []
val dates_in_month_2_res = dates_in_month([(1,2,2025), (11,3,2025)], 2) = [(1,2,2025)]
val dates_in_month_3_res = dates_in_month([(1,2,2025), (11,2,2025)], 2) = [(1,2,2025), (11,2,2025)]

val dates_in_months_1_res = dates_in_months([(1,1,2025), (4,2,2025), (4,3,2025)], [1,2])

val get_nth_1_res = valOf(get_nth(["string 1", "string 2"], 1)) = "string 1"
val get_nth_2_res = valOf(get_nth(["string 1", "string 2"], 2)) = "string 2"
val get_nth_3_res = valOf(get_nth(["string 1", "string 2", "string 3"], 2)) = "string 2"
val get_nth_4_res = valOf(get_nth(["string 1", "string 2", "string 3", "string 4"], 4)) = "string 4"

val date_to_string_1_res = date_to_string((10,9,2015)) = "September-10-2015"

val number_before_reaching_sum_1_res = number_before_reaching_sum(4, [1,1,3]) = 2
val number_before_reaching_sum_2_res = number_before_reaching_sum(10, [1,1,3, 3, 1, 2]) = 5
val number_before_reaching_sum_3_res = number_before_reaching_sum(70, [30,60,90,120]) = 1

val what_month_1_res = what_month(31) = 2
val what_month_2_res = what_month(70) = 3
val what_month_3_res = what_month(362) = 12

val month_range_1_res = month_range(12, 16) = [1,1,1,1,1]
val month_range_2_res = month_range(30, 32) = [1,1,2]

val oldest_1_res = valOf(oldest([(1,1,2025), (31,12,2024)])) = (31,12,2024)
val oldest_2_res = valOf(oldest([(25,12,2024), (31,12,2024)])) = (25,12,2024)

val cumulative_sum_1_res = cumulative_sum [12,27,13]

val number_in_months_challenge_1_res = 
  number_in_months_challenge([(1,2,2025), (12,2,2025), (15,6,2025)], [1,2,2,6]) = 3

val dates_in_months_challenge_1_res = 
  dates_in_months_challenge([(1,2,2025), (12,2,2025), (15,6,2025), (11,4,2024)], [1,2,2,6])