double precision function Shifted_Exponential_PDF(x, a, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
	if(x.gt.a) then
		Shifted_Exponential_PDF = b * dexp(-b * (x - a))
	else
		Shifted_Exponential_PDF = Zero
	endif
end function Shifted_Exponential_PDF
