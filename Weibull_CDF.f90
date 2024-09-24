!
double precision function Weibull_CDF(X, Weibull_Scale, AL, BETA)
  implicit none
  real*8 X, Weibull_Scale, AL, BETA
  Weibull_CDF = 1.0-dexp(-(((X-AL)/(Weibull_Scale-AL))**BETA))
  RETURN
END function Weibull_CDF
