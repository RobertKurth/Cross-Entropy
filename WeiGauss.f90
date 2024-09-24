!
	double precision FUNCTION WEIGauss(A, B, ETA, AL, Weibull_Beta)
!
implicit none
!
  real*8  Theta(4), W(4), A, B, ETA, AL, Weibull_Beta, error, X, Weibull_PDF
  integer I
  DATA Theta/-.86113631D+00, -.33998104D+00, .33998104D+00, .86113631D+00/
  DATA W/.34785485D+00, .65214515D+00, .65214515D+00, .34785485D+00/

  Theta(1) = -dsqrt(3.0D+00 / 7.0D+00 + 2.0D+00 / 7.0D+00 * dsqrt(6.0D+00 / 5.0D+00 ))
  Theta(2) = -dsqrt(3.0D+00 / 7.0D+00 - 2.0D+00 / 7.0D+00 * dsqrt(6.0D+00 / 5.0D+00 ))
  Theta(3) =  dsqrt(3.0D+00 / 7.0D+00 - 2.0D+00 / 7.0D+00 * dsqrt(6.0D+00 / 5.0D+00 ))
  Theta(4) =  dsqrt(3.0D+00 / 7.0D+00 + 2.0D+00 / 7.0D+00 * dsqrt(6.0D+00 / 5.0D+00 ))
  
	W(1) = (18.0D+00 - dsqrt(30.0D+00)) / 36.0D+00
	W(2) = (18.0D+00 + dsqrt(30.0D+00)) / 36.0D+00
	W(3) = (18.0D+00 + dsqrt(30.0D+00)) / 36.0D+00
	W(4) = (18.0D+00 - dsqrt(30.0D+00)) / 36.0D+00
	
	WEIGauss = 0.0D+00
  do I = 1, 4
		X = ((B-A)*Theta(I) + B + A)/2.0D+00
		WEIGauss = WEIGauss + W(I) * X * Weibull_PDF(X, ETA, AL, Weibull_Beta)
	enddo
	
	WEIGauss = (B-A)*WEIGauss/2.0D+00
  
	RETURN
  END
