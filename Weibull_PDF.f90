double precision function Weibull_PDF(x, eta, a, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	eta
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
	if(x.gt.eta.and.a.gt.Zero.and.b.gt.Zero) then
		Weibull_PDF = a * b * (b * (x - eta))**(a - One) * dexp(-((b * (x - eta))**a))
	else
		Weibull_PDF = Zero
	endif
end function Weibull_PDF
