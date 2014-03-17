

You are given the following information, but you may prefer to do some research for yourself.

    1 Jan 1900 was a Monday.
    Thirty days has September,
    April, June and November.
    All the rest have thirty-one,
    Saving February alone,
    Which has twenty-eight, rain or shine.
    And on leap years, twenty-nine.
    A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?


> import Data.List
> days = cycle ["Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"]
> numDays = 365*100 + 25 - 1

Number of days in a given month:
Jan: 31, Feb:28 or 29, Mar: 31, Apr: 30, May:31, June:30, July:31,
August:31, September:30, Oct:31, Nov:30, Dec:31

> dayMonths n =
>   [31,
>    if mod n 4 == 0 then 29 else 28,
>    31,
>    30,
>    31,
>    30,
>    31,
>    31,
>    30,
>    31,
>    30,
>    30]

> months :: [Int]
> months = concatMap dayMonths [1901..2000]
> splitMonths [] x = [x]
> splitMonths (a:as) b = let (x,y) = splitAt a b in x : splitMonths as y
> answer = (length . filter (\x -> head x == "Sunday")) (splitMonths months (drop 365 days))
