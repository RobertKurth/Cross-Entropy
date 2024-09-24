!
double precision function Simpson_Integration(A, B, Mean, Std_deviation)
	use Program_Constants
    implicit none
    real*8, intent(IN)          ::  A
    real*8, intent(IN)          ::  B
    real*8, intent(IN)          ::  Mean
    real*8, intent(IN)          ::  Std_deviation
    real*8                      ::  Global_interval
    real*8                      ::  x_left
    real*8                      ::  x_right
    real*8                      ::  DIV
    real*8                      ::  ERROR
    real*8                      ::  FIXED
    real*8                      ::  Half_interval
    real*8                      ::  Save_Simp
    real*8                      ::  SIMOLD
    real*8                      ::  SUM
    ! Real functions    
    real*8                      ::  Normal_PDF
    ! Local counters
    integer                     ::  Maximum_limit
    integer                     ::  Number_of_panels
    integer                     ::  I
    integer                     ::  J_index
    integer                     ::  K
    
    ERROR = 0.000001D+00
    Number_of_panels = 2
    Simpson_Integration = Normal_PDF(A, Mean, Std_deviation) + Normal_PDF(B, Mean, Std_deviation)
    Global_interval = 0.5*(A + B)
    
    DIV = (B-A)

    FIXED = Simpson_Integration
    Half_interval = Normal_PDF(Global_interval, Mean, Std_deviation)
    Simpson_Integration = Simpson_Integration + 4.0*Half_interval
    Simpson_Integration = Simpson_Integration*DIV/3.0
    FIXED = FIXED+2.0*Half_interval

1   SIMOLD = Simpson_Integration
    SUM = 0.0
    Maximum_limit = Number_of_panels / Two
    Number_of_panels = Number_of_panels * Two
    DO I = 1, Maximum_limit
        J_index = Number_of_panels - Two * I + 1
        K = Two * I - 1
        x_left = (J_index * A + K * B)/Number_of_panels
        x_right = (K * A + J_index * B)/Number_of_panels
        SUM = SUM + (Normal_PDF(x_left, Mean, Std_deviation) + Normal_PDF(x_right, Mean, Std_deviation))
    enddo
    Save_Simp = SUM
    SUM = 4.0 * SUM + FIXED
    FIXED = FIXED + Two * Save_Simp
    DIV = (B-A)/Number_of_panels
    Simpson_Integration = DIV*SUM/3.0
    IF(Number_of_panels.GT.5500) then
        return
    endif
    
    if(Simpson_Integration.EQ.0) then
        GOTO 1
    elseif(dabs((Simpson_Integration - SIMOLD) / Simpson_Integration).GT.ERROR) then
        GOTO 1
    elseif(Number_of_panels.LT.64) then
        GOTO 1
    endif

end function Simpson_Integration
