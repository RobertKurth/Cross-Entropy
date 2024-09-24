double precision function Q_SNL(x_80_C, Alpha_SNL, Beta_SNL)
	use Program_Constants
	implicit none
    real*8, intent(IN)                  ::  x_80_C
    real*8, intent(IN)                  ::  Alpha_SNL
    real*8, intent(IN)                  ::  Beta_SNL
    
    Q_SNL = -Universal_Gas_constant / (One / 353.15 - Tref_CISCC) * dlog(x_80_C / (Alpha_SNL * 50.0D+00**Beta_SNL))
    
end function Q_SNL