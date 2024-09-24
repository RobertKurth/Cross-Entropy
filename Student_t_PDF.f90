double precision function Student_t_PDF(x, mu, sigma, nu)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	mu      !	mean
    real*8, intent(IN)					::	sigma	! scale parameter (NOT standard deviation
    real*8, intent(IN)					::	nu      !	degrees of freedom

    Student_t_PDF = (One + One / nu * ((x - mu) / sigma)**2)**(One_half * (nu + One))

end function Student_t_PDF
