subroutine transpose(m, n, Z, Z_transpose)
    use Program_Constants
	implicit none
   
    integer,intent(IN)                              ::  m
    integer,intent(IN)                              ::  n
    integer                                         ::  J
    real*8, dimension(1:n, 1:m)                     ::  Z
    real*8, dimension(1:m, 1:n)                     ::  Z_transpose

    integer                                         ::  I

    Z_transpose(1:m, 1:n)= Zero
    
    do J = 1, n
        do I = 1, m
            Z_transpose(I, J) = Z(J, I)
        enddo
    enddo
    
end subroutine transpose