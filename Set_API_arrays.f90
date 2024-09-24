subroutine Set_API_arrays(Parameter_array)
	use Program_Constants
    implicit none   
    real*8, intent(OUT), dimension(6, 5, 5, 7)        ::  Parameter_array
    
    integer                                         ::  I
    integer                                         ::  J
    integer                                         ::  K
    integer                                         ::  L

    character*256                                    ::  Filename
    character*256                                    ::  CASE_TITLE

    Filename = '..\input\API_579_Parameters.txt'
    close(17)
    call Open_File_Read(17, Filename)
    read(17, *) CASE_TITLE
    !   
    !  D/t	 t/c    a/t     Gp(c)	    Gp(a)	    β	    H 
    !
    do I = 1, 6
        do J = 1, 5
            do K = 1, 5
                read(17, *) (Parameter_array(I, J, K, L), L = 1,  7)
            enddo
        enddo
    enddo
    
end subroutine Set_API_arrays