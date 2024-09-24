subroutine Pit_Locations(Next_Random_Number, Model, Number_of_pits, Cask_length, Cask_radius, Cask_thickness, Weld_thickness, &
        Pit_location, Distance, Pit_1, Pit_2)	
	!
	use Program_Constants
	implicit none
	!
	integer*4, intent(IN)													    ::	Next_Random_Number
	integer, intent(IN)														    ::	Model
	integer, intent(IN)														    ::	Number_of_pits
    real*8, intent(IN)														    ::	Cask_length
    real*8, intent(IN)														    ::	Cask_radius
    real*8, intent(IN)														    ::	Cask_thickness
    real*8, intent(IN)														    ::	Weld_thickness
    
    real*8, dimension(1:2, 1:Number_of_pits), intent(INOUT)					    ::	Pit_location		!	Pit(1, *) x location
																									    !	Pit(2, *) y location
    
    real*8, dimension(1:Number_of_pits, 1:Number_of_pits, 1:4), intent(INOUT)	::	Distance			!	Distance between pit CENTERS
	integer, intent(INOUT)                                                      ::  Pit_1
	integer, intent(INOUT)                                                      ::  Pit_2
	integer                                                                     ::  I
	integer                                                                     ::  J
    !   Real functions
    real*8                                                                      ::	Random_Real
    real*8                                                                      ::	Minimum_Distance

    do I = 1, Number_of_pits
        Pit_location(1, I) = Random_Real(Next_Random_Number) * Cask_length
        Pit_location(2, I) = Random_Real(Next_Random_Number) * Weld_thickness
    enddo
    
end subroutine Pit_Locations
    
double precision function Minimum_Distance(Number_of_pits, Distance, K, L)
	!
	use Program_Constants
	implicit none
	!
	integer, intent(IN)														    ::	Number_of_pits
	integer, intent(OUT)                                                        ::	K
	integer, intent(OUT)                                                        ::	L
    real*8, dimension(1:Number_of_pits, 1:Number_of_pits, 1:4)                  ::	Distance			!	Distance between pit CENTERS
	integer                                                                     ::  I
	integer                                                                     ::  J
	integer                                                                     ::  Model

    Minimum_Distance = 9.99D+99
    K = 1
    L = 2
    do I = 1, Number_of_pits
        do J = 1, Number_of_pits
            do Model = 1, 4
                if(Distance(I, J, Model).lt.Minimum_Distance.and.I.ne.J) then
                  Minimum_Distance = Distance(I, J, Model)
                  K = I
                  L = J
                endif
            enddo
        enddo
    enddo
    
end function Minimum_Distance
    
double precision function Get_Distance(x1, y1, x2, y2)
	!
	use Program_Constants
	implicit none
	!
	real*8, intent(IN)														    ::	x1
	real*8, intent(IN)														    ::	y1
	real*8, intent(IN)														    ::	x2
	real*8, intent(IN)														    ::	y2

    Get_Distance = dsqrt((x1 - x2)**2 + (y1 - y2)**2)
    
end function Get_Distance
            