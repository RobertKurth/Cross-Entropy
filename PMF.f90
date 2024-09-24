!
! Discrete PMFs
!
double precision function Bernoulli_PMF(x, p, b)
	! In reference [1] a is shown as p
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	p
    real*8, intent(IN)					::	b
	if(x.le.Zero.or.x.ge.One) then
		Bernoulli_PMF = Zero
	elseif(p.lt.Zero.or.p.gt.One) then
		Bernoulli_PMF = Zero
	else
		Bernoulli_PMF = p**x * (One - p)**(One - x)
	endif
end function Bernoulli_PMF
!
double precision function Binomial_PMF(x, p, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	p
    real*8, intent(IN)					::	b
	Binomial_PMF = 1
end function Binomial_PMF
!
double precision function Discrete_Uniform_PMF(n)
	use Program_Constants
	implicit none
    integer, intent(IN)					::	n
	Discrete_Uniform_PMF = One / dfloat(n)
end function Discrete_Uniform_PMF
!
double precision function Geometric_PMF(x, p, b)
	use Program_Constants
	implicit none
    real*8, intent(IN)					::	x
    real*8, intent(IN)					::	p
    real*8, intent(IN)					::	b
	Geometric_PMF = p * (One - p)**(x - One)
end function Geometric_PMF
!
double precision function Poisson_PMF(n, a, b)
	use Program_Constants
	implicit none
    integer, intent(IN)					::	n
    real*8, intent(IN)					::	a
    real*8, intent(IN)					::	b
		! Functions
		integer											::	factorial
	Poisson_PMF = dexp(-a) * a**n / factorial(n)
end function Poisson_PMF    
