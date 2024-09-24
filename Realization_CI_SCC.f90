subroutine Realization_CI_SCC(Variable_Name, Next_Random_Number, Number_of_Parameters, K_Aleatory, Aleatory, Number_Of_Variables, Parameters, Number_of_bins, DPD, &
        !Geometry
		R_Inner, R_Mean, R_Outer, Circumference, Cross_Section_Pipe, Moment_of_inertia, Area_of_Pipe, &
		Diameter, thickness, Alpha_SNL, Beta_SNL, x_80_C, Alpha_EPRI_shallow, Alpha_EPRI_transition, Alpha_EPRI_shallow_over_deep, Number_of_pits, &
        Relative_humidity, Yield, Ultimate, alpha_RO, Ramberg_Osgood_n, JI_C, K_threshold, Pit_depth, Pit_radius, Correlation_coefficent, &
        Random_variable_sample, Sampled_Bin)
	
	USE PORTLIB
	use Program_Constants
	!  USE IMSL
	!     ******************************************************************
	!    INPUT DATA FOR EACH REALIZATION OF MONTE CARLO SIMULATION
	!     ******************************************************************
	!
	implicit none
	integer*4																				::	Next_Random_Number
    
	integer, intent(IN)																		::	Number_of_Parameters
	integer, intent(IN)																		::	K_Aleatory
	integer, intent(IN)																		::	Aleatory
	integer, intent(IN)																		::	Number_of_bins
	integer, intent(IN)																		::	Number_Of_Variables
	integer, intent(IN)																		::	Number_of_pits                  ! Variable 18

    character(LEN=256), intent(IN), dimension(1:Number_Of_Variables)						::	Variable_Name
	character(LEN=256)																		::	Local_Variable_Name
    real*8, intent(IN), dimension(1:Number_Of_Variables, 1:Number_of_Parameters)			::	Parameters
    real*8, intent(IN), dimension(1:Number_of_Variables, 1:2, 1:Number_of_bins)				::	DPD
    integer, dimension(1:Number_Of_Variables, 1:Aleatory)									::	Sampled_bin
	
	real*8, intent(OUT)																		::	Diameter						! Variable 1
	real*8, intent(OUT)																		::	thickness						! Variable 2
	real*8, intent(OUT)																		::	Alpha_SNL						! Variable 3
	real*8, intent(OUT)																		::	Beta_SNL                        ! Variable 4
	real*8, intent(OUT)																		::	x_80_C							! Variable 5
	real*8, intent(OUT)																		::	Alpha_EPRI_shallow				! Variable 6
	real*8, intent(OUT)																		::	Alpha_EPRI_transition			! Variable 7
	real*8, intent(OUT)																		::	Alpha_EPRI_shallow_over_deep	! Variable 8
	real*8, intent(OUT)																		::	Relative_humidity               ! Variable 9
	real*8, intent(OUT)																		::	Yield							! Variable 11
	real*8, intent(OUT)																		::	Ultimate						! Variable 12
	real*8, intent(OUT)																		::	alpha_RO						! Variable 13
	real*8, intent(OUT)																		::	Ramberg_Osgood_n				! Variable 14
	real*8, intent(OUT)																		::	JI_C							! Variable 15
	real*8, intent(OUT)																		::	K_threshold						! Variable 15
    
    real*8, dimension(1:Number_of_pits, 1:4), intent(OUT)									::	Pit_depth						! Variable 16
    real*8, dimension(1:Number_of_pits, 1:4), intent(OUT)									::	Pit_radius						! Variable 17           
    real*8, intent(OUT)																		::	Correlation_coefficent          ! Variable 19
    
	real*8, intent(OUT)																		::	R_Inner							! Calculated value 
	real*8, intent(OUT)																		::	R_Mean 							! Calculated value
	real*8, intent(OUT)																		::	R_Outer							! Calculated value
	real*8, intent(OUT)																		::	Circumference 					! Calculated value
	real*8, intent(OUT)																		::	Cross_Section_Pipe 				! Calculated value
	real*8, intent(OUT)																		::	Moment_of_inertia				! Calculated value
	real*8, intent(OUT)																		::	Area_of_Pipe					! Calculated value
	real*8, intent(OUT), dimension(1:Number_Of_Variables, 1:2)                              ::	Random_variable_sample

	!Real functions
	real*8																					::	Random_Real
	real*8																					::	Sample
    real*8																					::	Correlated_Sample
	!Local reals
	real*8																					::	Parameter_1
	real*8																					::	Parameter_2
	real*8																					::	Parameter_3
	real*8																					::	F_Ramberg_Osgood
	real*8																					::	Ratio_U_Y
	real*8																					::	Ratio_Yield_Std_dev
	real*8																					::	Ratio_Ultimate_Std_dev
	!Local counters
	integer																					::	I
	integer																					::	Variable
	integer																					::	I_Type
	integer																					::	J
	integer																					::	K
	integer																					::	Model
	integer																					::	Pit_number
    

	!
	!		Because	of the Olson correlation if ANY of the variables associated with yield, Ultimate_pipe or modulus are outside
	!		the lower and upper limits ALL are resampled
	!
    
    !Distribution = Parameters(Variable, 1)
    !Mean_Value = Parameters(Variable, 2)
    !Standard_Deviation = Parameters(Variable, 3)
    !Lower_Bound = Parameters(Variable, 4)
    !Upper_Bound = Parameters(Variable, 5)
    !
    !	Uncertainty_type = Parameters(Variable, Number_of_Parameters)
	!
				
    Variable = 19
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Correlation_coefficent = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Correlation_coefficent
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 1
    if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Diameter = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Diameter
        Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif

	Variable = 2
    if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		thickness = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = thickness
        Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif

	R_Outer = Diameter * One_half
	R_Inner = (Diameter - Two * thickness) * One_half
	Circumference = pi * R_Inner * Two
	R_Mean = 0.5*(R_Outer + R_inner)
	Cross_Section_Pipe = (pi*(R_Outer**2-R_Inner**2))
	Moment_of_inertia = 0.25*pi*(R_outer**4-R_inner**4)
	Area_of_Pipe = (PI*(R_Outer**2-R_Inner**2))
		
	Variable = 3
	if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Alpha_SNL = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Alpha_SNL
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 4
	if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
			Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
			if(dabs(Parameters(Variable, 6)).gt.0.05D+00) then
                    
                I = int(Parameters(Variable, 1))
                K = int(Parameters(Variable, 7))
                J = int(Parameters(K, 1))
				if(int(Parameters(Variable, 7)).lt.Variable) then
					Beta_SNL = Correlated_Sample(Next_Random_Number, Random_variable_sample(K, 1), &
                        J, Parameters(K, 2), Parameters(K, 3), Parameters(K, 4), Parameters(K, 5), Correlation_coefficent, &
                        I, Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5))

					Random_variable_sample(Variable, 1) = Beta_SNL
					Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
				endif
			else
				Beta_SNL = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
					Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
				Random_variable_sample(Variable, 1) = Beta_SNL
				Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
			endif
		else
			Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
		endif
        Random_variable_sample(Variable, 1) = Beta_SNL
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 5
	if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		x_80_C = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = x_80_C
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 6
	if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Alpha_EPRI_shallow = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Alpha_EPRI_shallow
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 7
	if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Alpha_EPRI_transition = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Alpha_EPRI_transition
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif

    Variable = 8
	if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Alpha_EPRI_shallow_over_deep = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Alpha_EPRI_shallow_over_deep
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 9
	if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Relative_humidity = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Ramberg_Osgood_n
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 10
	if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Yield = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Yield
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 11
	if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Ultimate  = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Ultimate
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 12
	if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		alpha_RO = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = alpha_RO
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 13
	if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Ramberg_Osgood_n = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Ramberg_Osgood_n
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 14
	if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		JI_C = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = JI_C
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 15
	if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		K_threshold = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = K_threshold
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif
    
    do Pit_number = 1, Number_of_pits
		Variable = 16
		if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
			Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
			Pit_depth(Pit_number, 1) = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
				Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
			Random_variable_sample(Variable, 1) = Pit_depth(Pit_number, 1)
			Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
		else
			Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
		endif
    
		Variable = 17
		if(Parameters(Variable, Number_of_Parameters).eq.0.or.K_Aleatory.eq.1) then
			Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
			Pit_radius(Pit_number, 1) = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
				Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
			Random_variable_sample(Variable, 1) = Pit_radius(Pit_number, 1)
			Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
		else
			Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
        endif
		
		do Model = 2, 4
			Pit_radius(Pit_number, Model) = Pit_radius(Pit_number, 1)
        enddo
        
      enddo
    
  
700	format(1000(1x, 1PE12.5))       
end subroutine Realization_CI_SCC

