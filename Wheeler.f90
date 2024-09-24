subroutine Wheeler(Variable, Variable_Name, Next_Random_Number, K_Aleatory, Aleatory, Number_Of_Variables, Experimental_cycles, Parameters, &
        Number_of_bins, Sample_number, x_UWFM, stress_UWFM, DPD, Sampled_Bin, J_total_array, J_total, P_total, Iorientation, &
        !
        JIc, CFP_Factor, R_inner, thickness, Fatigue_C, Fatigue_n, Depth_in, Length_in, &
        Yield, Pressure_Cyclic, Pressure_Hydrotest, Bending, Retardation_Cycles)
	
	USE PORTLIB
	use Program_Constants
	!  USE IMSL
	!     ******************************************************************
	!    INPUT DATA FOR EACH REALIZATION OF MONTE CARLO SIMULATION
	!     ******************************************************************
	!
	implicit none
	integer*4																	::	Next_Random_Number
	
	integer, intent(IN)															::	Variable
	integer, intent(IN)															::	K_Aleatory
	integer, intent(IN)															::	Aleatory
	integer, intent(IN)															::	Number_of_bins
    integer, intent(IN)															::	Sample_number
	integer, intent(IN)															::	Number_Of_Variables
    integer, intent(IN)                                                         ::	Experimental_cycles
	character(LEN=256), intent(IN), dimension(1:Number_Of_Variables)			::	Variable_Name
	character(LEN=256)															::	Local_Variable_Name
    real*8, intent(INOUT), dimension(1:NData)                                   ::	x_UWFM
	real*8, intent(IN), dimension(1:NData)                                      ::	stress_UWFM
	real*8, dimension(1:NData)                                                  ::	WRS
    integer, intent(INOUT)														::	Retardation_Cycles
    
    real*8, intent(IN), dimension(1:Number_Of_Variables, 1:6)					::	Parameters
    real*8, intent(IN), dimension(1:Number_of_Variables, 1:2, 1:Number_of_bins)	::	DPD
    integer, intent(IN), dimension(1:Number_Of_Variables, 1:Aleatory)			::	Sampled_bin
    real*8, dimension(1:6, 0:250)                                               ::  J_total_array                                                   !
    real*8, dimension(0:250)                                                    ::  J_total                                                         !
    real*8, dimension(0:250)                                                    ::  P_total                                                         !
    real*8, dimension(1:Maximum_cycles, 1:Number_of_crack_outputs)              ::  crack_outputs                                                   !
    real*8, dimension(1:Maximum_cycles)                                         ::  Print_Cycles                                                   !
    real*8, dimension(1:Maximum_cycles)                                         ::  Look_up_array                                                   !
    real*8, dimension(1:2, 1:Maximum_cycles)                                    ::  Print_length                                                   !
    integer, intent(IN)                                                         ::  Iorientation
    real*8, intent(IN)															::	JIc
    
    real*8, intent(IN)															::	CFP_Factor
    real*8          															::	Pressure_ksi

    real*8, intent(IN)															::  R_inner
    real*8, intent(IN)															::	thickness
    real*8, intent(IN)															::	Fatigue_C
    real*8, intent(IN)															::	Fatigue_n
    real*8, intent(IN)															::	Depth_in
    real*8, intent(IN)															::	Length_in
    real*8, intent(IN)															::	Yield
    real*8, intent(IN)															::	Pressure_Cyclic
    real*8, intent(IN)															::	Pressure_Hydrotest 
    real*8															            ::	Stress_Cyclic
    real*8															            ::	Stress_Hydrotest 
    real*8, intent(IN)															::	Bending

    real*8																		::	K_max
    real*8																		::	K_overload
    real*8																		::	rovert
    real*8																		::	covera
    real*8																		::	aovert
    real*8																		::	Stress 
    real*8																		::	Depth 
    real*8																		::	Initial_Length 

    real*8																		::	SurfK90
    real*8																		::	SurfK0
    real*8																		::	SurfK90_overload
    real*8																		::	SurfK0_overload
    real*8																		::	Normalized_J
    real*8																		::	Normalized_J_overload
    real*8																		::	J_Normalized    
    
	!Real functions
	real*8																		::	Random_Real
	real*8																		::	Sample
	!Integer functions
	integer																		::	VLookUP
    integer																		::	LookUP_retardation
	!Local reals
	real*8																		::	Parameter_1
	real*8																		::	Parameter_2
	real*8																		::	Parameter_3
    real*8																		::	Membrane_stress
    real*8																		::	K_membrane_circ
    real*8																		::	K_bending_circ
    
	real*8																		::	Stress_for_K
	real*8																		::	R_outer
	real*8																		::	Cross_section_pipe
	real*8																		::	F_Ramberg_Osgood
	real*8																		::	Ratio_U_Y
	real*8																		::	Ratio_Yield_Std_dev
	real*8																		::	Ratio_Ultimate_Std_dev
    real*8																		::	Crack_Face_Pressure
    real*8																		::	Cp_cycle
    real*8																		::	Cp_overload
    real*8																		::	rp_i
    real*8																		::	Rp_overload
    real*8																		::	Rp_J_initial
    real*8																		::	Rp_J_overload
    real*8																		::	J_0
    real*8																		::	J_overload
    
    real*8																		::	Rp_overload_prime
    real*8																		::	dadN
    real*8																		::	dadN_cyclic
    real*8																		::	dadN_overload
	real*8																		::	Delta_K
	real*8																		::	Depth_cycle
	real*8																		::	current_crack_length
	real*8																		::	crack_length_cyclic
	real*8																		::	crack_length_overload
	real*8																		::	Depth_overload
	real*8																		::	Length_overload
    real*8																		::	Length_NO_overload
    real*8																		::	Length_post_overload
	!Local counters
	integer																		::	I
	integer																		::	J
	integer																		::	K
	integer																		::	Cycles
	integer																		::	Delta_Cycles
    integer																		::	m
    integer																		::	Ierror
    integer																		::	Pre_hydrotest_Cycles
    integer																		::	Cycle_Range
    logical                                                                     ::  TWC
    if(depth_in.le.0.98D+00 * thickness) then
        TWC = .true.
    else
        TWC = .false.
    endif
    
    Depth = Depth_in
    Initial_Length = Length_in
    rovert =  R_inner / thickness
    covera = Initial_Length / Depth
    aovert = Depth / thickness
    Crack_Face_Pressure = CFP_Factor * Pressure_cyclic
    Stress_Cyclic = Pressure_Cyclic * R_inner / thickness
    Stress = Stress_Cyclic + Crack_Face_Pressure
    m = Fatigue_n
    R_outer = R_inner + thickness
    Cross_Section_Pipe = pi * (R_outer**2 - R_inner**2)
    Membrane_stress = Zero
    dadN_overload = Zero

    Cycles = 0
    J = 0
    Delta_Cycles = 50
    crack_length_overload = Zero
    Cp_cycle = Zero
    SurfK0_overload = Zero
    
    crack_length_cyclic = Initial_Length
    Stress_for_K = Pressure_Cyclic *(pi * R_inner * R_inner) / Cross_Section_Pipe ! * 6.89475728D+00 + Crack_Face_Pressure + Membrane_Stres
    WRS = Stress_UWFM + Stress_for_K

1   Cycles = Cycles + Delta_Cycles
    
    J = J +1
    covera = crack_length_cyclic / Depth

    if(TWC) then
        call KTW_xLPR_2(R_Inner, thickness, crack_length_cyclic, Iorientation, Stress_for_K, Bending, SurfK0, &
            K_membrane_circ, K_bending_circ, Ierror)
    else
        call KPW(Thickness_transition, 1000.0D+00, R_Inner, thickness, Depth, crack_length_cyclic, Ndata, x_UWFM, WRS, &
            Iorientation, Bending, SurfK90, SurfK0, Ierror)
    endif
    !
    !	Assuming stress goes from zero to Stress_max
    !
        
    if(TWC) then
        Delta_K = SurfK0
        J_Normalized = dexp((Delta_K - 12.839D+00) / 3.3644)
    
        J_Normalized = 0.012D+00 * dexp(0.3241D+00 * Delta_K)
        
        dadN = Fatigue_C * Delta_K**Fatigue_n
        dadN_cyclic = dadN
        crack_length_cyclic = crack_length_cyclic + dadN * Delta_Cycles
        crack_outputs(J, 1) = Cycles
        crack_outputs(J, 2) = SurfK0
        crack_outputs(J, 3) = SurfK0_overload
        crack_outputs(J, 4) = dadN_cyclic
        crack_outputs(J, 5) = crack_length_cyclic
        crack_outputs(J, 6) = crack_length_overload
        crack_outputs(J, 7) = Cp_cycle
        crack_outputs(J, 8) = Depth
        crack_outputs(J, 9) = Delta_K
        crack_outputs(J, 10) = J_Normalized
    else
        Delta_K = SurfK90
        J_Normalized = dexp((Delta_K - 12.839D+00) / 3.3644)

        J_Normalized = 0.012D+00 * dexp(0.3241D+00 * Delta_K)
        
        dadN = Fatigue_C * Delta_K**Fatigue_n
        dadN_cyclic = dadN
        depth = depth + dadN * Delta_Cycles
        if(depth.ge.depth) then
            depth = dmin1(depth, thickness)
            !TWC = .true.
        endif
        
        Delta_K = SurfK0
        J_Normalized = dexp((Delta_K - 12.839D+00) / 3.3644)

        J_Normalized = 0.012D+00 * dexp(0.3241D+00 * Delta_K)

    Pressure_ksi = Pressure_Cyclic
    K = VLookUP(Pressure_ksi, P_total)
    Normalized_J = (J_total(K) * (Pressure_ksi - P_total(K - 1)) + J_total(K - 1) * (P_total(K) - Pressure_ksi)) / (P_total(K) - P_total(K - 1))
    Normalized_J = Normalized_J / Yield
    
        dadN = Fatigue_C * Delta_K**Fatigue_n
        dadN_cyclic = dadN
        crack_length_cyclic = crack_length_cyclic + dadN * Delta_Cycles
        crack_outputs(J, 1) = Cycles
        crack_outputs(J, 2) = SurfK0
        crack_outputs(J, 3) = SurfK0_overload
        crack_outputs(J, 4) = dadN_cyclic
        crack_outputs(J, 5) = crack_length_cyclic
        crack_outputs(J, 6) = crack_length_overload
        crack_outputs(J, 7) = Cp_cycle
        crack_outputs(J, 8) = Depth
        crack_outputs(J, 9) = Delta_K
        crack_outputs(J, 10) = Normalized_J
    endif
    
    current_crack_length = crack_length_cyclic
    crack_length_overload = current_crack_length
    write(15, '(*(G0.7,:,","))') (crack_outputs(J, K), K = 1, Number_of_crack_outputs)
    if(Cycles.le.Experimental_cycles) then
        goto 1
    else
        Length_NO_overload = crack_length_cyclic
    endif
    Pre_hydrotest_Cycles = Cycles
        
    crack_length_overload = Length_NO_overload
    !    
    !   These equations are from
    !
    !   Kudari, S., Maiti, B., & Ray, K. (2007)
    !   "The Effect of Specimen Geometry om Plastic Zone Sizes: A Study Using the J integral"
    !   Journal of Strain Analysis, 42, 123-136.    !   
    !   
    !   and use thee J values from the Emc-FE method together with the J-K correlation and the
    !   J-plastic zone size from this reference to define the relationship between K and Rp
    !
    Pressure_ksi = Pressure_Cyclic
    K = VLookUP(Pressure_ksi, P_total)
    Normalized_J = (J_total(K) * (Pressure_ksi - P_total(K - 1)) + J_total(K - 1) * (P_total(K) - Pressure_ksi)) / (P_total(K) - P_total(K - 1))
    Normalized_J = Normalized_J / Yield
    
    Pressure_ksi = Pressure_Hydrotest / thousand
    K = VLookUP(Pressure_ksi, P_total)
    Normalized_J_overload = (J_total(K) * (Pressure_ksi - P_total(K - 1)) + J_total(K - 1) * (P_total(K) - Pressure_ksi)) / (P_total(K) - P_total(K - 1))
    Normalized_J_overload = Normalized_J_overload / Yield
    
    if(Normalized_J_overload.le.0.02D+00) then
        Rp_J_overload = crack_length_overload * 91.0D+00 * Normalized_J_overload
    else
        Rp_J_overload = crack_length_overload * (0.0612 * dlog(Normalized_J_overload) + 0.8187D+00)
    endif

    if(.not.Plane_stress) then
        Rp_J_overload = Rp_J_overload / Three
    endif
    
    if(Normalized_J.le.0.02D+00) then
        rp_i = Length_overload * 91.0D+00 * Normalized_J
    else
        rp_i = Length_overload * (0.0612 * dlog(Normalized_J) + 0.8187D+00)
    endif
    
    Cycles = Pre_hydrotest_Cycles
!    ****    ****    ****    ****    ****    ****    ****    ****    ****    ****    ****!
!                                                                                        !
2   Cycles = Cycles + Delta_Cycles                                                       !
!                                                                                        !
!    ****    ****    ****    ****    ****    ****    ****    ****    ****    ****    ****!    
    J = J +1
    if(J.gt.Maximum_cycles) then
        J = J - 1
        Cycles = Cycles - Delta_Cycles
        goto 3
    endif
    covera = crack_length_overload / Depth

    if(TWC) then
        call KPW(Thickness_transition, 1000.0D+00, R_Inner, thickness, Depth, crack_length_overload, Ndata, x_UWFM, WRS, &
            Iorientation, Bending, SurfK90, SurfK0_overload, Ierror)
        call KTW_xLPR_2(R_Inner, thickness, crack_length_overload, Iorientation, Stress_for_K, Bending, SurfK0_overload, &
            K_membrane_circ, K_bending_circ, Ierror)
        call KPW(Thickness_transition, 1000.0D+00, R_Inner, thickness, Depth, crack_length_cyclic, Ndata, x_UWFM, WRS, &
            Iorientation, Bending, SurfK90, SurfK0, Ierror)
        call KTW_xLPR_2(R_Inner, thickness, crack_length_cyclic, Iorientation, Stress_for_K, Bending, SurfK0, &
            K_membrane_circ, K_bending_circ, Ierror)
    else
        call KPW(Thickness_transition, 1000.0D+00, R_Inner, thickness, Depth, crack_length_cyclic, Ndata, x_UWFM, WRS, &
            Iorientation, Bending, SurfK90, SurfK0, Ierror)
        call KPW(Thickness_transition, 1000.0D+00, R_Inner, thickness, Depth, crack_length_overload, Ndata, x_UWFM, WRS, &
            Iorientation, Bending, SurfK90, SurfK0_overload, Ierror)
    endif

    if(Normalized_J.le.0.02D+00) then
        rp_i = crack_length_overload * 91.0D+00 * Normalized_J
    else
        rp_i = crack_length_overload * (0.0612 * dlog(Normalized_J) + 0.8187D+00)
    endif
    
    Delta_K = SurfK0
    dadN = Fatigue_C * Delta_K**m    
    dadN_cyclic = dadN
    crack_length_cyclic = crack_length_cyclic + dadN * Delta_Cycles
    
    Cp_cycle = (rp_i / (Length_NO_overload + Rp_J_overload - crack_length_overload))**m                 ! Equation 2
    Rp_overload_prime = Rp_J_initial + Initial_Length                                                   ! Equation 5
    if(Cp_cycle.ge.One) then
        Retardation_Cycles = Cycles - Pre_hydrotest_Cycles
        goto 3
    endif
    
    Delta_K = SurfK0_overload
    dadN = Cp_cycle * Fatigue_C * Delta_K**m    
    dadN_overload = dadN

    crack_length_overload = crack_length_overload + dadN * Delta_Cycles
    J_Normalized = dexp((Delta_K - 12.839D+00) / 3.3644)
    
    J_Normalized = 0.012D+00 * dexp(0.3241D+00 * Delta_K)
    	

    crack_outputs(J, 1) = Cycles
    crack_outputs(J, 2) = SurfK0
    crack_outputs(J, 3) = SurfK0_overload
    crack_outputs(J, 4) = dadN_cyclic
    crack_outputs(J, 5) = crack_length_cyclic
    crack_outputs(J, 6) = crack_length_overload
    crack_outputs(J, 7) = Cp_cycle
    crack_outputs(J, 8) = Depth
    crack_outputs(J, 9) = Delta_K
    crack_outputs(J, 10) = Normalized_J
    
    write(15, '(*(G0.7,:,","))') (crack_outputs(J, K), K = 1, Number_of_crack_outputs)
	goto 2
3   continue
    Cycle_Range = min(Maximum_cycles, Cycles)
    do I = 1, Cycle_Range
        Print_Cycles(I) = I
        Print_length(1, I) = crack_outputs(I, 5)
        Print_length(2, I) = crack_outputs(I, 6)
    enddo
    Pressure_ksi = Pressure_Cyclic
    Look_up_array(1:Cycle_Range) = crack_outputs(1:Cycle_Range, 6)
    I = LookUP_retardation(Cycle_Range, crack_length_overload, Look_up_array)
    Look_up_array(1:Cycle_Range) = crack_outputs(1:Cycle_Range, 5)
    J = LookUP_retardation(Cycle_Range, crack_length_overload, Look_up_array)
    
    Retardation_Cycles = crack_outputs(I, 1) - crack_outputs(J, 1)
    !write(15, '(*(G0.7,:,","))') Retardation_Cycles, Cp_cycle
    write(16, '(*(G0.7,:,","))') Sample_number, Retardation_Cycles, Depth_in, Length_in, Fatigue_C, Fatigue_n, Pressure_Cyclic, Pressure_Hydrotest, &
        Delta_K, J_Normalized
    !Cp_cycle = (Rp_J_initial / (Length_overload + Rp_overload_prime - current_crack_length))**m        ! Equation 6
    if(Sample_number.eq.1.or.int(Sample_number / 100) * 100.eq. Sample_number) then
        write(*, 700) Sample_number, Retardation_Cycles
    endif
    
700 format(1x,'Sample: ',I6,' Retardation cycles: ',I7)
end subroutine Wheeler
    