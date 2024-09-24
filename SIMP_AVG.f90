!
	double precision function Normal_Simpson_Integration(x_Left, x_Right, Mean, Std_dev)
   	use Program_Constants		!
    implicit none
    real*8, intent(IN)          ::  x_Left
    real*8, intent(IN)          ::  x_Right
    real*8, intent(IN)          ::  Mean
    real*8, intent(IN)          ::  Std_dev
    real*8                      ::  Midpoint
    real*8                      ::  Normal_midpoint
    real*8                      ::  XL
    real*8                      ::  XR
    real*8                      ::  Divisor
    real*8                      ::  ERROR
    real*8                      ::  Simpson_updated
    real*8                      ::  HALF
    real*8                      ::  Panel_limit
    real*8                      ::  Simpson_new
    real*8                      ::  Simpson_previous
    real*8                      ::  Sum
    integer                     ::  Number_of_Panels
    integer                     ::  I
    integer                     ::  J
    integer                     ::  K
    !
    real*8                      ::  Normal_PDF
    real*8                      ::  CMOM_X

    ERROR = 1.0D-12
    Number_of_Panels = 2
    Normal_Simpson_Integration = x_Left * Normal_PDF(x_Left, Mean, Std_dev) + x_Right * CMOM_X(x_Right, Mean, Std_dev)
    Midpoint = 0.5D+00 * (x_Left + x_Right)
    
    Divisor = One_half * (x_Right - x_Left)
    
    Normal_midpoint = CMOM_X(Midpoint, Mean, Std_dev)
    Normal_Simpson_Integration = Normal_Simpson_Integration + Four * Normal_midpoint
    Normal_Simpson_Integration = Normal_Simpson_Integration * Divisor / Three
    Simpson_updated = Simpson_updated + Two * Normal_midpoint
1   Simpson_previous = Normal_Simpson_Integration
    Sum = Zero
    Panel_limit = Number_of_Panels / 2
    Number_of_Panels = Number_of_Panels * 2
    do I = 1, Panel_limit
        J = Number_of_Panels - 2 * I + 1
        K = 2 * I - 1
        XL = (J * x_Left + K * x_Right) / Number_of_Panels
        XR = (K * x_Left + J * x_Right) / Number_of_Panels
        Sum = Sum + (xL * Normal_PDF(xL, x_Left, x_Right) + xR * Normal_PDF(xR, x_Left, x_Right))
    enddo
    Simpson_new = Sum
    Sum = Four * Sum + Simpson_updated
    Simpson_updated = Simpson_updated + Two * Simpson_new
    Divisor = (x_Right - x_Left) / Number_of_Panels
    Normal_Simpson_Integration = Divisor * Sum / Three
    if(Number_of_Panels.GT.1000000) then
        return
    endif
    if(Normal_Simpson_Integration.EQ.0) goto 1
    if(DABS((Normal_Simpson_Integration - Simpson_previous) / Normal_Simpson_Integration).GT.ERROR.or.Number_of_Panels.LT.128) then
        goto 1
    endif
end function Normal_Simpson_Integration