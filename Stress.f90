double precision function Stress(thickness, Pressure, Pit_depth, Pit_radius, xot, Stress_UWFM)
	use Program_Constants
	implicit none
    real*8, intent(IN)															::	thickness
    real*8, intent(IN)															::	Pressure
    real*8, intent(IN)															::	Pit_depth
    real*8, intent(IN)															::	Pit_radius
	real*8, intent(IN), dimension(1:NData)                                      ::	xot
	real*8, intent(IN), dimension(1:NData)                                      ::	Stress_UWFM

    integer                                                                     ::  I
    !Real functions
    real*8                                                                      ::  linear_interpolation
    
    do I = 1, Ndata
        if(Pit_depth / thickness.lt.xot(I)) then
            goto 1
        endif
    enddo
1   continue
    if(Pit_depth / thickness.lt.One) then
        Stress = linear_interpolation(Pit_depth / thickness, xot(I - 1), xot(I), Stress_UWFM(I - 1), Stress_UWFM(I))
    else
        Stress = Stress_UWFM(NData)
    endif
    
    !Stress = Stress / Ten
end function Stress