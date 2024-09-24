!
double precision function Sample(Next_Random_Number, Number_of_Variables, Iv, Number_of_bins, Variable_Name, Distribution_Type, Mean_Value, Standard_Deviation, &
    X_Lower_in, X_Upper_in, DPD, Sampled_bin)	
	!
	use Program_Constants
	implicit none
	!
	integer, intent(IN)																	::	Next_Random_Number
	integer, intent(IN)																	::	Iv
	integer, intent(IN)																	::	Number_of_bins
	integer, intent(IN)																	::	Distribution_Type
    integer, intent(IN)																	::	Number_of_Variables
    integer, intent(INOUT)                                                              ::	Sampled_bin
    
	real*8, intent(IN)																	::	Mean_Value
	real*8, intent(IN)																	::	Standard_Deviation
	real*8, intent(IN)																	:: 	X_Lower_in          ! Lower bound for random variable
	real*8, intent(IN)																	:: 	X_Upper_in			! Upper bound for random variable
    real*8, intent(IN), dimension(1:Number_of_Variables, 1:2, 1:Number_of_bins)			::	DPD

    CHARACTER*256, intent(IN)															::	Variable_Name
		
	integer																				::	Counter
	integer																				::	ISeed
	integer																				::	I
	real*8																				::	Random_Real
    
	real*8 Parameter_1, Parameter_2, Parameter_3, U1, U2
	real*8 T_Lower, T_Middle, T_Upper, Denom_gt_T_Middle, F_at_T_Middle, Sample_gt_T_Middle
	real*8 X_Lower, X_Upper

	integer Sample_DPD
	real*8, dimension(1:Number_of_bins)													:: AH

	X_Lower = X_Lower_in
	X_Upper = X_Upper_in
    
    Counter = 0
  
	if(Distribution_Type.eq.0) then
			Sample = Mean_Value
			goto 9999
	endif
	
	if(Number_of_bins.ne.0) then
		GOTO 1000
	endif
		
	Counter = 0
	!
	!	 1: Uniform
	!  2: Normal
	!  3: Lognormal
	!  4: Weibull
	!  5: Triangular
	!  6: Exponential
	!  7: Gumbel
	!  8: U-Quadratic
	!  9: Log-logistic
	! 10: DPD
	!

	GOTO (100, 200, 300, 400, 500, 600, 700, 800, 900, 1000) iabs(Distribution_Type)
  
100   CONTINUE
!
!UNIFORM Distribution_Type
!
    Sample = (X_Upper - X_Lower) * Random_Real(Next_Random_Number) + X_Lower
	return
		
200	CONTINUE
    !
    !NORMAL Distribution_Type
    !
		
    U1 = Random_Real(Next_Random_Number)
    U2 = Random_Real(Next_Random_Number)
    Sample = DSQRT(-dlog(U1*U1))*dcos(2.0*pi*U2)
    IF(Distribution_Type.gt.0) then
        Parameter_1 = Mean_Value
        Parameter_2 = Standard_Deviation
	elseif(Distribution_Type.lt.Zero) then
		!
		!LOGNORMAL Distribution_Type
		!
		call Change_Parameter(Mean_Value, Standard_Deviation, X_Lower_in, X_Upper_in, Distribution_Type, Parameter_1, Parameter_2, Parameter_3, X_Lower, X_Upper)
    endif

    if(Distribution_Type.gt.Zero) then
        Parameter_1 = Mean_Value
        Parameter_2 = Standard_Deviation
	elseif(Distribution_Type.lt.Zero) then
		!
		!LOGNORMAL Distribution_Type
		!
		call Change_Parameter(Mean_Value, Standard_Deviation, X_Lower_in, X_Upper_in, Distribution_Type, Parameter_1, Parameter_2, Parameter_3, X_Lower, X_Upper)
	endif
    Sample = Sample * Parameter_2 + Parameter_1
    if(Distribution_Type.lt.Zero) then
        Sample = dexp(Sample)
    endif
    
	IF(Sample.LT.X_Lower_in.OR.Sample.GT.X_Upper_in) THEN
		Counter = Counter + 1
		if(Counter.gt.2000) then
			write(*, 702) Iv, Variable_Name  
        read(*, *) I
			stop
		endif
		GOTO 200
	endif
		
	goto 9999
300	continue
		GOTO 200
400 CONTINUE
    !
    !WEIBULL Distribution_Type
    !
		!	Scale: Mean_Value
		!	Shape: Standard_Deviation
    U1 = Random_Real(Next_Random_Number)
    Sample = (-dlog(1-U1))**(1.0/Standard_Deviation) * Mean_Value + X_Lower
		if(Distribution_Type.eq.4) then
			IF(Sample.LT.X_Lower.OR.Sample.GT.X_Upper) THEN
				Counter = Counter + 1
				if(Counter.gt.2000) then
					write(*, 702) Iv, Variable_Name  
            read(*, *) I
					stop
				endif
				U1 = Random_Real(Next_Random_Number)
				GOTO 400
			endif
		endif
		goto 9999
500 CONTINUE
    !
    !TRIANGULAR Distribution_Type
    !
		T_Lower = X_Lower
		T_Middle = Mean_Value
		T_Upper = X_Upper
		
    Denom_gt_T_Middle = (T_Upper - T_Lower) * (T_Upper - T_Middle)
    U1 = Random_Real(Next_Random_Number)
    F_at_T_Middle = (T_Middle - T_Lower) / (T_Upper - T_Lower)
    if(U1.lt.F_at_T_Middle) then
        Sample = T_Lower + dsqrt(U1 * (T_Upper - T_Lower) * (T_Middle - T_Lower))
    else
        Sample = T_Upper - dsqrt((One - U1) * (T_Upper - T_Lower) * (T_Upper - T_Middle))
    endif
    IF(Sample.LT.X_Lower.OR.Sample.GT.X_Upper) THEN
			Counter = Counter + 1
			if(Counter.gt.2000) then
				write(*, 702)   Iv, Variable_Name
            read(*, *) I
				stop
			endif
			
      GOTO 500
    endif
    goto 9999

600	CONTINUE
		!
		!	Exponential
		!
    U1 = Random_Real(Next_Random_Number)
    Sample = -dlog(1-U1) / Mean_Value
    IF(Sample.LT.X_Lower.OR.Sample.GT.X_Upper) THEN
			Counter = Counter + 1
			if(Counter.gt.2000) then
				write(*, 702) Iv, Variable_Name
            read(*, *) I
				stop
			endif
      GOTO 600
		endif
		goto 9999
700	CONTINUE
		!
		!	Gumbel
		!
		call Change_Parameter(Mean_Value, Standard_Deviation, X_Lower_in, X_Upper_in, Distribution_Type, Parameter_1, Parameter_2, Parameter_3, X_Lower, X_Upper)
    U1 = Random_Real(Next_Random_Number)
		    
		Sample = Parameter_2 * (-dlog(-dlog(U1))) + Parameter_1
		
    IF(Sample.LT.X_Lower.OR.Sample.GT.X_Upper) THEN
			Counter = Counter + 1
			if(Counter.gt.2000) then
				write(*, 702)   Iv, Variable_Name, Variable_Name
            read(*, *) I
				stop
			endif
      GOTO 700
		endif
		goto 9999
800	CONTINUE
		!
		! U-Quadratic
		!
		Parameter_2 = Standard_Deviation * dsqrt(6.0D+00) / pi
		Parameter_1 = Mean_Value - 0.57721D+00 * Parameter_1
		U1 = Random_Real(Next_Random_Number)
		Sample = Parameter_1 + Parameter_2 * dlog(-dlog(1.0D+00 - U1))
    goto 9999
900	continue
		!
		!	Log-Logistic
		!
		call Change_Parameter(Mean_Value, Standard_Deviation, X_Lower_in, X_Upper_in, Distribution_Type, Parameter_1, Parameter_2, Parameter_3, X_Lower, X_Upper)
    U1 = Random_Real(Next_Random_Number)
		Sample = Parameter_1 / (1.0D+00 / U1 - 1) ** (1.0D+00 / Parameter_2)
		goto 9999
1000 continue
		 !
		 !	DPD SAMPLING
		 !
1001 continue
		!
		!     DPD METHOD USED FOR SAMPLING
		!
			 
		do I = 1, Number_of_bins
			AH(I) = DPD(Iv, 1, I)
		enddo
			 
		 Sample = DPD(Iv, 1, Sampled_bin)
1002 continue
		 IF(Sample.LT.X_Lower.OR.Sample.GT.X_Upper) THEN
			Counter = Counter + 1
			if(Counter.gt.1000) then
				!IS = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
				write(*, 702) Iv, Variable_Name  
            read(*, *) I
				!stop
				goto 9999
			endif
				Sampled_bin = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
			goto 1000
		 endif
9999 continue		 
701 format(1x,'For variable ',I3,2x,A80)				
702 format(1x,'Sample within the input bounds could not be found for variable ',I3,2x,A80)				
end function Sample