!
! Continuous Scores
!
!
double precision function Normal_Score(x, a, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
    ! Real functions
	Normal_Score = dexp(-One_half * ((x - a) / b)**2) / (b * dsqrt(Two * pi))
end function Normal_Score
!
double precision function Gamma_Score(x, a, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
    ! Real functions
    real*8								::	Gamma_Param
	if(x.gt.Zero) then
		Gamma_Score = dexp(-b * x) * x**(a - One) * b**a / Gamma_Param(a)
	else
		Gamma_Score = Zero
	endif
end function Gamma_Score
!
double precision function Exponential_Score(x, a, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
	if(x.gt.Zero) then
		Exponential_Score = a * dexp(-a * x)
	else
		Exponential_Score = Zero
	endif
end function Exponential_Score
!
double precision function Double_Exponential_Score(x, a, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
	Double_Exponential_Score = One_half * b * dexp(-b * dabs(x - a))
end function Double_Exponential_Score
!
double precision function Shifted_Exponential_Score(x, a, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
	if(x.gt.a) then
		Shifted_Exponential_Score = b * dexp(-b * (x - a))
	else
		Shifted_Exponential_Score = Zero
	endif
end function Shifted_Exponential_Score
!
double precision function Truncated_Exponential_Score(x, a, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
	if(x.ge.Zero.and.x.le.b) then
		Truncated_Exponential_Score = a * dexp(-a * x) / (One - dexp(-a * b))
	else
		Truncated_Exponential_Score = Zero
	endif	
end function Truncated_Exponential_Score
!
double precision function Beta_Score(x, a, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
    ! Real functions
    real*8								::	Gamma_Param

    if(x.ge.Zero.and.x.le.One) then
		Beta_Score = Gamma_Param(a + b) / Gamma_Param(a) / Gamma_Param(b) * x ** (a - One) * (One - x)**(b - One)
	else
		Beta_Score = Zero
	endif	
end function Beta_Score
!
double precision function Weibull_Score(x, a, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
	if(x.gt.Zero.and.a.gt.Zero.and.b.gt.Zero) then
		Weibull_Score = a * b * (b * x)**(a - One) * dexp(-((b * x)**a))
	else
		Weibull_Score = Zero
	endif
end function Weibull_Score
!
double precision function Pareto_Score(x, a, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
	if(x.gt.Zero.and.a.gt.Zero.and.b.gt.Zero) then
		Pareto_Score = a * b * (One + b * x)**(-(a + One)) * dexp(-((b * x)**a))
	else
		Pareto_Score = Zero
	endif
end function Pareto_Score
!
! Discrete Scores
!
double precision function Bernoulli_Score(x, a, b)
	! In reference [1] a is shown as p
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
	if(x.le.Zero.or.x.ge.One) then
		Bernoulli_Score = Zero
	elseif(a.lt.Zero.or.a.gt.One) then
		Bernoulli_Score = Zero
	else
		Bernoulli_Score = a**x * (One - a)**(One - x)
	endif
end function Bernoulli_Score
!
double precision function Binomial_Score(x, a, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
	!Binomial_Score = 
end function Binomial_Score
!
double precision function Discrete_Uniform_Score(x, a, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
	!Discrete_Uniform_Score = 
end function Discrete_Uniform_Score
!
double precision function Geometric_Score(x, a, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
	!Geometric_Score = 
end function Geometric_Score
!
double precision function Poisson_Score(x, a, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
	!Poisson_Score = 
end function Poisson_Score
!

		