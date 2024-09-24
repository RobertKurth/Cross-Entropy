double precision function Qf(aoc)
! Steven Polasik, Carl Jazske, Thomas Bubenik
!   "Review of Engineering Fracture Mechanis Model for Pipeline Applications"
!   Proc of 2016 11th Internaional Pipeline Conference (IPC)
!   September 2016  Calagary, Alberta, CA
	use Program_Constants
	implicit none
    real*8              ::  aoc
    real*8              ::  aot

    Qf = 1.2581D+00 - 0.20589D+00 * aoc -11.493D+00 * aoc**2 + 29.586D+00 * aoc** 3 - 23.584D+00 * aoc**4            !   equation (7)
    
end function Qf