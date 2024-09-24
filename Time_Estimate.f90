subroutine Time_Estimate(Year, Month, Day, Finish_time, Current_total_time, Day_1, Day_2, Hour_1, Hour_2, Minute_1, Minute_2, Seconds_1, Seconds_2)
	!
	use Program_Constants
	implicit none
	!
	integer*4								::	Hundredths_seconds, Year, Day_add, Hour_add, Minute_add, Seconds_add

	integer									::	Month
	integer									::	Day
	integer									::	Hour
	integer									::	Minute
	integer									::	Seconds
	
	character*1								::	Month_1
	character*1								::	Day_1
	character*1								::	Hour_1
	character*1								::	Minute_1
	character*1								::	Seconds_1
	
	character*1								::	Month_2
	character*1								::	Day_2
	character*1								::	Hour_2
	character*1								::	Minute_2
	character*1								::	Seconds_2
	character*2								::	am_pm
	
	real*8									::	Current_total_time
	real*8									::	Finish_time
	character*1, dimension(1:11)            ::	Number_character

	DATA Number_character /'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'/


	if(Finish_time.GT.3600.0D+00*24.0D+00) then
		Finish_time = dmin1(777600.0D+00, Finish_time)
		Day  = int(Finish_time/(3600.0D+00*24.0D+00))
		Hour = int((Finish_time - Day*3600*24)/3600.0D+00)
		Minute = int((Finish_time- Day*3600*24 - Hour*3600)/60.0)
		Seconds = int(Finish_time - Day*3600*24- Hour*3600.0D+00 -Minute*60.0D+00)
	elseif(Finish_time.GT.3600.0D+00) then
		Day = 0
		Hour = int(Finish_time/3600.0D+00)
		Minute = int((Finish_time - Hour*3600)/60.0)
		Seconds = int(Finish_time - Hour*3600.0D+00 -Minute*60.0D+00)
	elseif(Finish_time.GT.60.0D+00) then
		Day = 0
		Hour = 0
		Minute  = int(Finish_time/60.0D+00)
		Seconds = int(Finish_time - Minute*60.0D+00)
	else
		Day = 0
		Hour = 0
		Minute = 0
		Seconds = int(Finish_time)
	endif
							
	Seconds = Seconds+Seconds
	if(Seconds.gt.60) then
		Minute = Minute+1
		Seconds = Seconds - 60
	endif
							
	Minute = Minute+Minute
	if(Minute.gt.60) then
		Hour = Hour+1
		Minute = Minute - 60
	endif
							
	Day = Day + Day
	Hour = Hour + Hour
	if(Hour.gt.24) then
		Day = Day+1
		Hour = Hour - 24
	endif              
	am_pm = 'am'
	if(Hour.gt.12) then
		Hour = Hour - 12
		am_pm = 'pm'
	endif
							
	if(Month.gt.9) then
		Month_1 = Number_character(2)
		Month_2 = Number_character(max(1, (Month-10) + 1))
	else
		Month_1 = Number_character(1)
		Month_2 = Number_character(Month + 1)
	endif                
							
	if(Day.gt.9) then
		Day_1 = Number_character(INT(Day/10) + 1)
		Day_2 = Number_character(max(1, Day-10*INT(Day/10) + 1))
	else
		Day_1 = Number_character(1)
		Day_2 = Number_character(Day+1)
	endif     
							
	if(Hour.gt.9) then
		Hour_1 = Number_character(INT(Hour/10) + 1)
		Hour_2 = Number_character(max(1, Hour-10*INT(Hour/10) + 1))
	else
		Hour_1 = Number_character(1)
		Hour_2 = Number_character(Hour+1)
	endif                
							
	if(Minute.gt.9) then
		Minute_1 = Number_character(INT(Minute/10) + 1)
		Minute_2 = Number_character(max(1, (Minute-10*INT(Minute/10)) + 1))
	else
		Minute_1 = Number_character(1)
		Minute_2 = Number_character(Minute+1)
	endif                
							
	if(Seconds.gt.9) then
		Seconds_1 = Number_character(INT(Seconds/10) + 1)
		Seconds_2 = Number_character(max(1, (Seconds-10*INT(Seconds/10)) + 1))
	else
		Seconds_1 = Number_character(1)
		Seconds_2 = Number_character(Seconds+1)
    endif                
    
end subroutine Time_Estimate
    