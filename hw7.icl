module hw7
import StdEnv

:: Date = { year :: Int
           ,month :: Month
           ,day :: Int }
           
:: Month = January | February | March | April | May | June | July | August | September | October | November | December

MonthtoNumber :: Month -> Int
MonthtoNumber January = 1
MonthtoNumber February = 2
MonthtoNumber March = 3
MonthtoNumber April = 4
MonthtoNumber May =5
MonthtoNumber June =6
MonthtoNumber July=7
MonthtoNumber August=8
MonthtoNumber September=9
MonthtoNumber October=10
MonthtoNumber November=11
MonthtoNumber December=12

dayscount :: Month -> Int
dayscount  January = 31
dayscount  February = 28
dayscount  March = 31
dayscount  April = 30
dayscount  May =31
dayscount  June =30
dayscount  July=31
dayscount  August=30
dayscount  September=31
dayscount  October=30
dayscount  November=31
dayscount  December=30



NumbertoMonth :: Int -> Month
NumbertoMonth 1= January
NumbertoMonth 2= February
NumbertoMonth 3= March
NumbertoMonth 4= April
NumbertoMonth 5= May
NumbertoMonth 6= June
NumbertoMonth 7= July
NumbertoMonth 8= August
NumbertoMonth 9= September
NumbertoMonth 10= October
NumbertoMonth 11= November
NumbertoMonth 12= December

GetMonthNum :: Date  -> Int
GetMonthNum {month} = MonthtoNumber month
//Start = GetMonthNum {year = 2018 , month = January ,day = 28}

//ChangeN :: Date Int -> Date
//ChangeN d x = {d & month = NumbertoMonth x}
//Start = ChangeN {year = 2018 , month = January ,day = 28} 3


//ChangePersonName :: Date Int -> Date
//ChangePersonName person newnumber = {person & name = newnumber}

//1. is_leap_year y determines whether year y is a leap year. In a leap year, month february has 29 days instead of the usual 28. A year is
//a leap year if it is dividable by 4, except if it is a multiple of 100 that is not dividable by 400. For instance, 1600 is a leap year,
//but 1700 is not a leap year.

//2. no_of_days y m determines the number of days of month m in year y (taking leap years into account).

//3. yesterday and tomorrow that return the date of the previous day and next day respectively (taking leap years into account).

is_leap_year :: Int -> Bool
is_leap_year x
|(x rem 100 == 0 && x rem 400 == 0)  =  True
|(x rem 4 == 0) && (x rem 100) <> 0 = True
= False
//Start = is_leap_year 2016
//Start = is_leap_year 1700


//is_leap_year year =

yesterday :: Date -> Date
yesterday {year,month,day}
|day == 1 && (dayscount month)== 31 && (MonthtoNumber month) <> 3 && (MonthtoNumber month) <> 1  = {year = year,month = NumbertoMonth(((MonthtoNumber month)+11) rem 12),day = 30}
|day == 1 && (dayscount month) == 30 = {year = year,month = NumbertoMonth(((MonthtoNumber month)+11) rem 12),day = 31}
|day == 1 && ((dayscount month) == 28 || (dayscount month) == 28) = {year = year,month = NumbertoMonth(((MonthtoNumber month)+11) rem 12),day = 30}
|day == 1 && (MonthtoNumber month) == 3 = {year = year,month = NumbertoMonth(((MonthtoNumber month)+11) rem 12),day = 28}
|day == 1 && (MonthtoNumber month) == 1 = {year = year-1,month = December,day = 31} 
 = {year = year,month = month,day = day-1}

//Start = yesterday {year=2015,month=March,day=1}
//Start = yesterday {year=2015,month=July,day=1}
//Start = yesterday {year=2019,month=January,day=1}
//Start = yesterday {year=2018,month=December,day=31}

tomorrow :: Date -> Date
tomorrow {year,month,day} 
|day == 31 && (dayscount month)== 31 && (MonthtoNumber month) <> 12 = {year = year,month = NumbertoMonth(((MonthtoNumber month)+13) rem 12),day = 1}
|day == 30 && (dayscount month)== 30 = {year = year,month = NumbertoMonth(((MonthtoNumber month)+13) rem 12),day = 1}
|day == 31 && (MonthtoNumber month) == 12 = {year = year+1,month = NumbertoMonth(((MonthtoNumber month)+13) rem 12),day = 1}
|day == 28 && not (is_leap_year year) && (dayscount month)== 28 = {year = year,month = NumbertoMonth(((MonthtoNumber month)+13) rem 12),day = 1}
|day == 29 && (is_leap_year year) && (dayscount month)== 28 = {year = year,month = NumbertoMonth(((MonthtoNumber month)+13) rem 12),day = 1}
= {year = year,month = month,day = day+1}
//Start = tomorrow {year=2015,month=March,day=31}
//Start = tomorrow {year=2017,month=December,day=31}
//Start = tomorrow {year=2001,month=February,day=28}
//Start = tomorrow {year=2000,month=February,day=29}

no_of_days :: Int Month -> Int
no_of_days y m
|(is_leap_year y) && (MonthtoNumber m) == 2 = (dayscount  m) + 1
= (dayscount  m)
//Start = no_of_days 2017 February
//Start = no_of_days 2017 January

