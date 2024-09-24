double precision function Secant_Bulging_Factor(Half_Crack_length, thickness)
! Steven Polasik, Carl Jazske, Thomas Bubenik
!   "Review of Engineering Fracture Mechanis Model for Pipeline Applications"
!   Proc of 2016 11th Internaional Pipeline Conference (IPC)
!   September 2016  Calagary, Alberta, CA
	use Program_Constants
    implicit none

    real*8, intent(IN)          ::  Half_Crack_length   ! full-length
    real*8, intent(IN)          ::  thickness
    
    Secant_Bulging_Factor = 0.25D+00 * Half_Crack_length**2 / (Half_Crack_length * thickness)
    Secant_Bulging_Factor = dsqrt(One + 1.255D+00 * Secant_Bulging_Factor - 0.0135D+00 * Secant_Bulging_Factor**2)
    
end function Secant_Bulging_Factor