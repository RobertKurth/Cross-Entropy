double precision function SNL_K_CISCC(stress, Y, depth)
    use Program_Constants
    implicit none
    real*8, intent(IN )              ::  stress
    real*8, intent(IN )              ::  Y
    real*8, intent(IN )              ::  depth
    
    SNL_K_CISCC = stress * Y * dsqrt(pi * depth)    !   MPa-m**0.5
    
end function SNL_K_CISCC
    