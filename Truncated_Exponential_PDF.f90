double precision function Truncated_Exponential_PDF(x, a, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
	if(x.ge.Zero.and.x.le.b) then
		Truncated_Exponential_PDF = a * dexp(-a * x) / (One - dexp(-a * b))
	else
		Truncated_Exponential_PDF = Zero
	endif	
end function Truncated_Exponential_PDF
