double precision function Reletive_Humidity_Critical(Temperature_C, Reletive_Humidity)
    use Program_Constants
    implicit none
    real*8, intent(IN )              ::  Temperature_C
    real*8, intent(IN )              ::  Reletive_Humidity
    
    Reletive_Humidity_Critical = (33.67D+00 - 7.974D-03 * Temperature_C -1.090D-03 * Temperature_C) / Hundred - Reletive_Humidity
    
end function Reletive_Humidity_Critical
    