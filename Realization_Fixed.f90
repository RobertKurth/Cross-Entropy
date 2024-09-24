subroutine Realization_Fixed(Variable_Name, Next_Random_Number, K_Aleatory, Aleatory, Number_Of_Variables, Parameters, Number_of_bins, DPD, &
        !Geometry
		R_Inner, R_Mean, R_Outer, Circumference, Cross_Section_Pipe, Moment_of_inertia, Area_of_Pipe, &
		Diameter, thickness, Crack_depth_multiplier, Crack_length_multiplier, MAOP, SMYS_stress_ksi, Yield, Ultimate, alpha, Ramberg_Osgood_n,  &
		Charpy_Upper_Shelf, JI_CT, JI_Sent, JR_vs_a, JI_1mm, SMYS_72, Point_aot, Point_toc, OD_over_t, Pressure, &
        Precrack_cycles_1, P_max_pre_crack_1, P_min_pre_crack_1, Overload_max, Overload_min, Cyclic_cycles, P_max_cyclic, P_min_cyclic, &
        Fatigue_C, Fatigue_n, Pressure_Hydrotest, Experimental_cycles, C_multiplier, m_multiplier, WRS_Inner, &
        Random_variable_sample, Sampled_Bin)
	
	USE PORTLIB
	use Program_Constants
	!  USE IMSL
	!     ******************************************************************
	!    INPUT DATA FOR EACH REALIZATION OF MONTE CARLO SIMULATION
	!     ******************************************************************
	!
	implicit none
	integer*4, intent(IN)                                                                   ::	Next_Random_Number
	
	integer, intent(IN)																		::	K_Aleatory
	integer, intent(IN)																		::	Aleatory
	integer, intent(IN)																		::	Number_of_bins
	integer, intent(IN)																		::	Number_Of_Variables
	character(LEN=256), intent(IN), dimension(1:Number_Of_Variables)						::	Variable_Name
	character(LEN=256)																		::	Local_Variable_Name
    real*8, intent(IN), dimension(1:Number_Of_Variables, 1:6)								::	Parameters
    real*8, intent(IN), dimension(1:Number_of_Variables, 1:2, 1:Number_of_bins)				::	DPD
    integer, dimension(1:Number_Of_Variables, 1:Aleatory)									::	Sampled_bin
	
	real*8, intent(OUT)																		::	Diameter						! Variable 1
	real*8, intent(OUT)																		::	thickness						! Variable 2
	real*8, intent(OUT)																		::	Crack_depth_multiplier			! Variable 3
	real*8, intent(OUT)																		::	Crack_length_multiplier			! Variable 4
	real*8, intent(OUT)																		::	MAOP							! Variable 5
    real*8, intent(OUT)																		::	SMYS_stress_ksi
	real*8, intent(OUT)																		::	Yield							! Variable 6
	real*8, intent(OUT)																		::	Ultimate						! Variable 7
	real*8, intent(OUT)																		::	alpha							! Variable 8
	real*8, intent(OUT)																		::	Ramberg_Osgood_n                ! Variable 9
	real*8, intent(OUT)																		::	Charpy_Upper_Shelf				! Variable 10
	real*8, intent(OUT)																		::	JI_CT							! Variable 11
	real*8, intent(OUT)																		::	JI_Sent							! Variable 12
	real*8, intent(OUT)																		::	JR_vs_a							! Variable 13
	real*8, intent(OUT)																		::	JI_1mm							! Variable 14
	real*8, intent(OUT)																		::	SMYS_72							! Variable 15
	real*8, intent(OUT)																		::	Point_aot                       ! Variable 16
	real*8, intent(OUT)																		::	Point_toc                       ! Variable 17
	real*8, intent(OUT)																		::	OD_over_t                       ! Variable 18
	real*8, intent(OUT)																		::	Pressure						! Variable 19
	real*8, intent(OUT)																		::	Fatigue_C						! Variable 20
	real*8, intent(OUT)																		::	Fatigue_n						! Variable 21
	real*8, intent(OUT)																		::	Pressure_Hydrotest				! Variable 22
    integer, intent(OUT)																	::	Experimental_cycles				! Variable 23
    real*8, intent(OUT)																		::	Precrack_cycles_1				! Variable 24
    real*8, intent(OUT)																		::	P_max_pre_crack_1				! Variable 25
    real*8, intent(OUT)																		::	P_min_pre_crack_1				! Variable 26
    real*8, intent(OUT)																		::	Overload_max					! Variable 27
    real*8, intent(OUT)																		::	Overload_min					! Variable 28
    real*8, intent(OUT)																		::	Cyclic_cycles					! Variable 29
    real*8, intent(OUT)																		::	P_max_cyclic					! Variable 30
    real*8, intent(OUT)																		::	P_min_cyclic					! Variable 31
    real*8, intent(OUT)																		::	C_multiplier					! Variable 32
    real*8, intent(OUT)																		::	m_multiplier					! Variable 33
	real*8, intent(OUT)																		::	WRS_Inner						! Variable 34
    
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
    !	Uncertainty_type = Parameters(Variable, 6)
	!
	Variable = 1
    if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Diameter = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Diameter
        Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif

    Variable = 18
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
		Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		OD_over_t = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
		Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
		Random_variable_sample(Variable, 1) = OD_over_t
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif

  Variable = 2
	if(Parameters(Variable, 2).gt.Zero) then
		Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		if(Parameters(Variable, 3).eq.0.or.K_Aleatory.eq.1) then
            Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
			thickness = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
                Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))

			OD_over_t = Diameter / thickness
			Random_variable_sample(Variable, 1) = OD_over_t
			Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
      
		else
            Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
		endif
	else
		Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		thickness = Diameter / OD_over_t
		Random_variable_sample(Variable, 1) = OD_over_t
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(18, K_Aleatory))
	endif
	
	Random_variable_sample(Variable, 1) = thickness
	R_Outer = Diameter * One_half
	R_Inner = (Diameter - Two * thickness) * One_half
	Circumference = pi * R_Inner * Two
	R_Mean = 0.5*(R_Outer + R_inner)
	Cross_Section_Pipe = (pi*(R_Outer**2-R_Inner**2))
	Moment_of_inertia = 0.25*pi*(R_outer**4-R_inner**4)
	Area_of_Pipe = (PI*(R_Outer**2-R_Inner**2))
		
	Variable = 3
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Crack_depth_multiplier = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Crack_depth_multiplier
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 4
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Crack_length_multiplier = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Crack_length_multiplier
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 5
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		MAOP = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = MAOP
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 6
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Yield = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Yield
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 7
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Ultimate = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Ultimate
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif
1   continue
    SMYS_stress_ksi = 0.72D+00 * Yield
	if(Parameters(6, 1).ne.Zero.and.Parameters(7, 1).eq.Zero) then
		Ratio_U_Y = Parameters(7, 2) / Parameters(6, 2)
		Ratio_Yield_Std_dev = Parameters(6, 3) / Parameters(6, 2)
		Ratio_Ultimate_Std_dev = Parameters(7, 3) / Parameters(7, 2)
		Ultimate = Ratio_U_Y * Yield
	endif
	IF(Yield.GT.0.95 * Ultimate.and.(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1)) THEN
		Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
				Variable = 6
				Yield = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
				Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
		Random_variable_sample(Variable, 1) = Yield
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
				Variable = 7
				Ultimate = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
			Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
		Random_variable_sample(Variable, 1) = Ultimate
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
		goto 1
	endif
	
	Variable = 8
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		alpha = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = alpha
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 9
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Ramberg_Osgood_n = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Ramberg_Osgood_n
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 10
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Charpy_Upper_Shelf = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Charpy_Upper_Shelf
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 11
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		JI_CT = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = JI_CT
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 12
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		JI_SENT  = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = JI_SENT
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 13
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		JR_vs_a = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = JR_vs_a
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 14
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		JI_1mm = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = JI_1mm
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 15
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		SMYS_72 = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = SMYS_72
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 16
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Point_toc = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Point_toc
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 17
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Point_aot = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Point_aot
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 19
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Pressure = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Pressure
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 20
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Fatigue_C = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Fatigue_C
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 21
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Fatigue_n = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Fatigue_n
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
	endif
		
	Variable = 22
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Pressure_Hydrotest = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Pressure_Hydrotest
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif
    
	Variable = 23
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Experimental_cycles = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Experimental_cycles
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif
        
	Variable = 24
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Precrack_cycles_1 = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Precrack_cycles_1
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif
    
    
	Variable = 25
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		P_max_pre_crack_1 = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = P_max_pre_crack_1
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif
    
    
	Variable = 26
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		P_min_pre_crack_1 = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = P_min_pre_crack_1
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif
    
	Variable = 27
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Overload_max = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Overload_max
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif
    
    
	Variable = 28
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Overload_min = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Overload_min
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif
    
    
	Variable = 29
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		Cyclic_cycles = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = Cyclic_cycles
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif
    
	Variable = 30
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		P_max_cyclic = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = P_max_cyclic
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif
    
	Variable = 31
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		P_min_cyclic = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = P_min_cyclic
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif
    
	Variable = 32
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		C_multiplier = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = C_multiplier
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif
    
	Variable = 33
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		m_multiplier = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = m_multiplier
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif
    
    Variable = 34
	if(Parameters(Variable, 6).eq.0.or.K_Aleatory.eq.1) then
        Sampled_bin(Variable, K_Aleatory) = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
		WRS_Inner = Sample(Next_Random_Number, Number_of_Variables, Variable, Number_of_bins, Variable_Name(Variable), int(Parameters(Variable, 1)), &
            Parameters(Variable, 2), Parameters(Variable, 3), Parameters(Variable, 4), Parameters(Variable, 5), DPD, Sampled_bin(Variable, K_Aleatory))
        Random_variable_sample(Variable, 1) = WRS_Inner
		Random_variable_sample(Variable, 2) = DPD(Variable, 2, Sampled_bin(Variable, K_Aleatory))
    else
        Sampled_bin(Variable, K_Aleatory) = Sampled_bin(Variable, K_Aleatory - 1)
    endif

    return
	!Alpha = Two / Thousand * E / Yield

	!Ramberg_Osgood_n = dexp(1.05487525D+00 + 0.003054D+00 * Yield - 0.000572243D+00 * Ultimate)
	!F_Ramberg_Osgood = (Yield**(Ramberg_Osgood_n-1.0D0)*E / Alpha) ** (1.0D+00 / Ramberg_Osgood_n)
	!Alpha = Yield**(Ramberg_Osgood_n-1.0D0)*E/F_Ramberg_Osgood**Ramberg_Osgood_n    
		
700	format(1000(1x, 1PE12.5))       
end subroutine Realization_Fixed

