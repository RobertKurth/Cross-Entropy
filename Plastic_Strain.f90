double precision function Plastic_Strain(Nominal_stress, Yield, Ultimate)
! Steven Polasik, Carl Jazske, Thomas Bubenik
!   "Review of Engineering Fracture Mechanis Model for Pipeline Applications"
!   Proc of 2016 11th Internaional Pipeline Conference (IPC)
!   September 2016  Calagary, Alberta, CA
	use Program_Constants
    implicit none
    real*8, intent(IN)          ::  Nominal_stress
    real*8, intent(IN)          ::  Yield
    real*8, intent(IN)          ::  Ultimate
    ! Real functions
    real*8                      ::  m
    real*8                      ::  n

    m = n(Yield, Ultimate)
    Plastic_Strain = (0.005D+00 - Yield / E) * (Nominal_stress / Yield)**(m)    !equation (15)
    
end function Plastic_Strain