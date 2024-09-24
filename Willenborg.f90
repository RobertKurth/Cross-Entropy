subroutine Willenborg(Variable, Variable_Name, Next_Random_Number, K_Aleatory, Aleatory, Fatigue_Model, Number_Of_Variables, Experimental_cycles, Parameters, &
        Number_of_bins, Sample_number, x_UWFM, stress_UWFM, DPD, Sampled_Bin, J_total_array, J_total, P_total, Iorientation, &
        Precrack_cycles_1, P_max_pre_crack_1, P_min_pre_crack_1, Overload_max, Overload_min, Cyclic_cycles, P_max_cyclic, P_min_cyclic, &
        !
        JIc, CFP_Factor, R_inner, thickness, Fatigue_C, Fatigue_n, Depth_in, Length_in, C_multiplier, m_multiplier, WRS_Inner, &
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
    integer, intent(IN)															::	Fatigue_Model
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
    real*8, dimension(1:6, 0:250)                                               ::  J_total_array
    real*8, dimension(0:250)                                                    ::  J_total      
    real*8, dimension(0:250)                                                    ::  P_total      
    real*8, dimension(1:Maximum_cycles, 1:Number_of_crack_outputs)              ::  crack_outputs
    real*8, dimension(1:Maximum_cycles)                                         ::  Print_Cycles
    real*8, dimension(1:Maximum_cycles)                                         ::  Look_up_array
    real*8, dimension(1:2, 1:Maximum_cycles)                                    ::  Print_length
    integer, intent(IN)                                                         ::  Iorientation
    real*8, intent(IN)															::	JIc
    
    real*8, intent(IN)															::	CFP_Factor
    real*8, intent(IN)															::	C_multiplier
    real*8, intent(IN)															::	m_multiplier
    real*8, intent(IN)															::	WRS_Inner
    
    real*8          															::	Pressure_ksi

    real*8, intent(IN)															::  R_inner
    real*8, intent(IN)															::	thickness
    real*8          															::	Fatigue_C
    real*8          															::	Fatigue_n
    real*8, intent(IN)															::	Depth_in
    real*8, intent(IN)															::	Length_in
    real*8, intent(IN)															::	Yield
    real*8, intent(IN)															::	Pressure_Cyclic
    real*8, intent(IN)															::	Pressure_Hydrotest 
    real*8																		::	Precrack_cycles_1	! Variable 24
    real*8																		::	P_max_pre_crack_1	! Variable 25
    real*8																		::	P_min_pre_crack_1	! Variable 26
    real*8																		::	Overload_max		! Variable 27
    real*8																		::	Overload_min		! Variable 28
    real*8																		::	Cyclic_cycles		! Variable 29
    real*8																		::	P_max_cyclic		! Variable 30
    real*8																		::	P_min_cyclic		! Variable 31
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
    real*8																		::	Delta_K90
    real*8																		::	K90_max
    real*8																		::	K90_min
    real*8																		::	Delta_K0
    real*8																		::	K0_max
    real*8																		::	K0_min

    real*8																		::	SurfK90_overload
    real*8																		::	SurfK0_overload
    real*8																		::	Normalized_J
    real*8																		::	Normalized_J_overload
    real*8																		::	J_Normalized    
    
	!Real functions
	real*8																		::	Random_Real
	real*8																		::	Sample
    real*8																		::	Walker
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
    
	real*8																		::	Stress_for_K_max
	real*8																		::	Stress_for_K_overload
	real*8																		::	Stress_for_K_min
	real*8																		::	R_outer
	real*8																		::	Cross_section_pipe
	real*8																		::	F_Ramberg_Osgood
	real*8																		::	Ratio_U_Y
	real*8																		::	Ratio_Yield_Std_dev
	real*8																		::	Ratio_Ultimate_Std_dev
    real*8																		::	Crack_Face_Pressure
    real*8																		::	Cp_cycle
    real*8																		::	Cp_overload
    real*8																		::	Rp_cycle
    real*8																		::	Rp_overload
    real*8																		::	Dcp_overload
    real*8																		::	Rp_J_initial
    real*8																		::	Rp_J_overload
    real*8																		::	J_0
    real*8																		::	J_overload
    
    real*8																		::	Rp_overload_prime
    real*8																		::	dadN
    real*8																		::	dadN_90
    real*8																		::	dadN_0
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
    real*8																		::	R
    real*8																		::	mm2_in2
    real*8																		::	C_retardation
    real*8																		::	Max_Delta_Error
    !Local counters
	integer																		::	I
	integer																		::	J
	integer																		::	K
	integer																		::	Cycles
    integer																		::	Hold_Cycles
	integer																		::	Delta_Cycles
    real*8																		::	m
    integer																		::	Ierror
    integer																		::	Pre_hydrotest_Cycles
    integer																		::	Cycle_Range
    logical                                                                     ::  TWC
    logical                                                                     ::  Retardation
    
    if(depth_in.ge.0.98D+00 * thickness) then
        TWC = .true.
    else
        TWC = .false.
    endif
    Retardation = .true.
    Retardation = .false.
    mm2_in2 = dsqrt(25.4D+00)
    Max_Delta_Error =0.00001D+00
    
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
    Rp_J_initial = Zero
    C_retardation = One

    Cycles = 0
    J = 0
    crack_length_overload = Zero
    Cp_cycle = Zero
    SurfK90_overload = Zero
    SurfK0_overload = Zero
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   Pre-crack phase
    !                                                                                                                   !   Pre-crack phase
    !   Precrack cycles                                                                                                 !   Pre-crack phase
    !                                                                                                                   !   Pre-crack phase
    Delta_Cycles = 1000
    crack_length_cyclic = Initial_Length * One_Half                                                                     !   Pre-crack phase
    current_crack_length = crack_length_cyclic

    Hold_Cycles = Cycles
    do J = 1, Precrack_cycles_1, Delta_Cycles                                                                           !   Pre-crack phase
1       continue
        Cycles = Hold_Cycles
        Cycles = Cycles + Delta_Cycles                                                                                  !   Pre-crack phase
        SurfK90 = Zero                                                                                                  !   Pre-crack phase
        Stress_for_K_max = P_max_pre_crack_1 * 0.00689475728D+00                                                        !   Pre-crack phase
        
        WRS = Stress_UWFM + Stress_for_K_max                                                                            !   Pre-crack phase
        if(TWC) then                                                                                                    !   Pre-crack phase
            call KTW_xLPR_2(R_Inner, thickness, crack_length_cyclic, Iorientation, Stress_for_K_max, Bending, K0_max, & !   Pre-crack phase
                K_membrane_circ, K_bending_circ, Ierror)                                                                !   Pre-crack phase
        else                                                                                                            !   Pre-crack phase
            call KPW(R_Inner, thickness, Depth, crack_length_cyclic, x_UWFM, WRS, Iorientation, &                       !   Pre-crack phase
                Bending, K90_max, K0_max, Ierror)                                                                       !   Pre-crack phase
        endif                                                                                                           !   Pre-crack phase
        SurfK90 = Zero                                                                                                  !   Pre-crack phase
        Stress_for_K_min = P_min_pre_crack_1 * 6.89475728D-03                                                           !   Pre-crack phase
        WRS = Stress_UWFM + Stress_for_K_min                                                                            !   Pre-crack phase
        if(TWC) then                                                                                                    !   Pre-crack phase
            call KTW_xLPR_2(R_Inner, thickness, crack_length_cyclic, Iorientation, Stress_for_K_min, Bending, K0_min, & !   Pre-crack phase
                K_membrane_circ, K_bending_circ, Ierror)                                                                !   Pre-crack phase
        else                                                                                                            !   Pre-crack phase
            call KPW(R_Inner, thickness, Depth, crack_length_cyclic, x_UWFM, WRS, Iorientation, &                       !   Pre-crack phase
                Bending, K90_min, K0_min, Ierror)                                                                       !   Pre-crack phase
        endif                                                                                                           !   Pre-crack phase
    !                                                                                                                   !   Pre-crack phase
        R = K90_min / K90_max                                                                                           !   Pre-crack phase
        if(Depth / thickness.gt.0.95D+00) then                                                                          !   Pre-crack phase
            Depth = thickness                                                                                           !   Pre-crack phase
            TWC = .true.                                                                                                !   Pre-crack phase
            goto 5                                                                                                      !   Pre-crack phase
        else                                                                                                            !   Pre-crack phase
            Delta_K90 = (K90_max - K90_min) * 9.10048D-01                                                               !   Pre-crack phase
            dadN_90 = Walker(C_multiplier, m_multiplier, Fatigue_Model, C_retardation, Fatigue_C, Delta_K90, R, m, &
                Fatigue_n)                          !   Pre-crack phase
            !   Pre-crack phase
        endif                                                                                                           !   Pre-crack phase
                                                                                                                        !   Pre-crack phase
        Delta_K0 = (K0_max - K0_min) * 9.10048D-01                                                                      !   Pre-crack phase
        R = K0_min / K0_max                                                                                             !   Pre-crack phase
        dadN_0 = Walker(C_multiplier, m_multiplier, Fatigue_Model, C_retardation, Fatigue_C, Delta_K0, Zero, m, &       !   Pre-crack phase
            Fatigue_n)                                                                                                  !   Pre-crack phase
        if(Delta_Cycles.gt.2) then                                                                                      !   Pre-crack phase
            if(dadN_0 * Delta_Cycles / crack_length_cyclic.gt.Max_Delta_Error) then                                     !   Pre-crack phase
                Delta_Cycles = Delta_Cycles / 2                                                                         !   Pre-crack phase
                Cycles = Hold_Cycles                                                                                    !   Pre-crack phase
                goto 1                                                                                                  !   Pre-crack phase
            endif                                                                                                       !   Pre-crack phase
        endif                                                                                                           !   Pre-crack phase
                                                                                                                        !   Pre-crack phase
        Depth = Depth + dadN_90 * Delta_Cycles                                                                          !   Pre-crack phase
        crack_length_cyclic = crack_length_cyclic + dadN_0 * Delta_Cycles                                               !   Pre-crack phase
                                                                                                                        !   Pre-crack phase
        if(Sample_number.le.10) then
            write(15, '(*(G0.7,:,","))') Cycles, K90_min, K90_max, K0_min, K0_max, &                                    !   Pre-crack phase
                Delta_K90, Delta_K0, Depth, crack_length_cyclic                                                         !   Pre-crack phase
        endif
        Hold_Cycles = Cycles                                                                                            !   Pre-crack phase
                                                                                                                        !   Pre-crack phase
    enddo                                                                                                               !   Pre-crack phase
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   Pre-crack phase
    !                                                                                                                   !   Pre-crack phase
    !                           E N D   O F   P R E - C R A C K   P H A S E                                             !   Pre-crack phase
    !                                                                                                                   !   Pre-crack phase
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   Pre-crack phase
    !                                                                                                                   !   HYDROTEST phase
    !   Use the Walker equation for growing the crack during the HYDROTEST phase                                        !   HYDROTEST phase
    !                                                                                                                   !   HYDROTEST phase
    !                                                                                                                   !   HYDROTEST phase
    Stress_for_K_overload = Overload_max * 6.89475728D-03                                                               !   HYDROTEST phase
    WRS = Stress_UWFM + Stress_for_K_overload                                                                           !   HYDROTEST phase
    Length_NO_overload = crack_length_cyclic                                                                            !   HYDROTEST phase
    do I = 1, Experimental_cycles                                                                                           !   HYDROTEST phase
        Cycles = Cycles + Delta_Cycles                                                                                      !   HYDROTEST phase
                                                                                                                            !   HYDROTEST phase
        J = J +1                                                                                                            !   HYDROTEST phase
        covera = crack_length_cyclic / Depth                                                                                !   HYDROTEST phase
                                                                                                                            !   HYDROTEST phase
        if(TWC) then                                                                                                        !   HYDROTEST phase
            call KTW_xLPR_2(R_Inner, thickness, crack_length_cyclic, Iorientation, Stress_for_K_overload, Bending, SurfK0, &!   HYDROTEST phase
                K_membrane_circ, K_bending_circ, Ierror)                                                                    !   HYDROTEST phase
        else                                                                                                                !   HYDROTEST phase
            call KPW(R_Inner, thickness, Depth, crack_length_cyclic, x_UWFM, WRS, Iorientation, &                           !   HYDROTEST phase
                Bending, SurfK90, SurfK0, Ierror)                                                                           !   HYDROTEST phase
        endif                                                                                                               !   HYDROTEST phase
                                                                                                                            !   HYDROTEST phase
        if(Depth / thickness.gt.0.95D+00) then                                                                              !   HYDROTEST phase
            Depth = thickness                                                                                               !   HYDROTEST phase
            TWC = .true.                                                                                                    !   HYDROTEST phase
        else                                                                                                                !   HYDROTEST phase
            Delta_K90 = SurfK90 * 9.10048D-01                                                                               !   HYDROTEST phase
            dadN_90 = Walker(C_multiplier, m_multiplier, Fatigue_Model, C_retardation, Fatigue_C, Delta_K90, R, m, &        !   HYDROTEST phase
                Fatigue_n)                                                                                                  !   HYDROTEST phase
            Depth = Depth + dadN_90 * Delta_Cycles                                                                          !   HYDROTEST phase
        endif                                                                                                               !   HYDROTEST phase
                                                                                                                            !   HYDROTEST phase
        Delta_K0 = SurfK0 * 9.10048D-01                                                                                     !   HYDROTEST phase
        dadN_0 = Walker(C_multiplier, m_multiplier, Fatigue_Model, C_retardation, Fatigue_C, Delta_K0, R, m, Fatigue_n)     !   HYDROTEST phase
        Depth = dmin1(thickness, Depth + dadN_90 * Delta_Cycles)                                                            !   HYDROTEST phase
        crack_length_overload = crack_length_cyclic + dadN_0 * Delta_Cycles                                                 !   HYDROTEST phase
    enddo
                                                                                                                            !   HYDROTEST phase
    Dcp_overload = crack_length_overload - Length_NO_overload                                                               !   HYDROTEST phase
    Rp_overload = (Delta_K0 / Yield)**2 / pi + Length_NO_overload                                                           !   HYDROTEST phase
                                                                                                                            !   HYDROTEST phase
    crack_length_cyclic = current_crack_length
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
        Rp_J_overload = crack_length_overload * (One + 91.0D+00 * Normalized_J_overload)
    else
        Rp_J_overload = crack_length_overload * (one + (0.0612 * dlog(Normalized_J_overload) + 0.8187D+00))
    endif

    if(.not.Plane_stress) then
        Rp_J_overload = Rp_J_overload / Three
    endif
    
    Pre_hydrotest_Cycles = Cycles
!    ****    ****    ****    ****    ****    ****    ****    ****    ****    ****    ****!
!                                                                                        !
    Hold_Cycles = Cycles
3   continue
    Cycles = Cycles + Delta_Cycles                                                       !
!                                                                                        !
!    ****    ****    ****    ****    ****    ****    ****    ****    ****    ****    ****!    

    J = J +1
    if(J.gt.Maximum_cycles) then
        J = J - 1
        Cycles = Cycles - Delta_Cycles
        goto 4
    endif
    covera = crack_length_overload / Depth
    SurfK90 = Zero                                                                                                  !   Cyclic phase
    Stress_for_K_max = P_max_cyclic * 6.89475728D-03                                                                !   Cyclic phase
    WRS = Stress_UWFM + Stress_for_K_max                                                                            !   Cyclic phase
    if(TWC) then                                                                                                    !   Cyclic phase
        call KTW_xLPR_2(R_Inner, thickness, crack_length_cyclic, Iorientation, Stress_for_K_max, Bending, K0_max, & !   Cyclic phase
            K_membrane_circ, K_bending_circ, Ierror)                                                                !   Cyclic phase
    else                                                                                                            !   Cyclic phase
        call KPW(R_Inner, thickness, Depth, crack_length_cyclic, x_UWFM, WRS, Iorientation, &                       !   Cyclic phase
            Bending, K90_max, K0_max, Ierror)                                                                       !   Cyclic phase
    endif                                                                                                           !   Cyclic phase
                                                                                                                    !   Cyclic phase
    Pressure_ksi = P_max_cyclic / Thousand                                                                          !   Cyclic phase
    K = VLookUP(Pressure_ksi, P_total)                                                                              !   Cyclic phase
    Normalized_J = (J_total(K) * (Pressure_ksi - P_total(K - 1)) + J_total(K - 1) * (P_total(K) - Pressure_ksi)) &  !   Cyclic phase
        / (P_total(K) - P_total(K - 1))                                                                             !   Cyclic phase
    Normalized_J = Normalized_J / Yield                                                                             !   Cyclic phase
                                                                                                                    !   Cyclic phase
    Stress_for_K_min = P_min_cyclic * 6.89475728D-03                                                                !   Cyclic phase
    WRS = Stress_UWFM + Stress_for_K_min                                                                            !   Cyclic phase
    if(TWC) then                                                                                                    !   Cyclic phase
        call KTW_xLPR_2(R_Inner, thickness, crack_length_cyclic, Iorientation, Stress_for_K_min, Bending, K0_min, & !   Cyclic phase
            K_membrane_circ, K_bending_circ, Ierror)                                                                !   Cyclic phase
    else                                                                                                            !   Cyclic phase
        call KPW(R_Inner, thickness, Depth, crack_length_cyclic, x_UWFM, WRS, Iorientation, &                       !   Cyclic phase
            Bending, K90_min, K0_min, Ierror)                                                                       !   Cyclic phase
    endif                                                                                                           !   Cyclic phase
                                                                                                                    !   Cyclic phase
    Delta_K0 = (K0_max - K0_min) * 9.10048D-01                                                                      !   Cyclic phase
    !if(C_retardation.lt.One) then                                                                                  !   Cyclic phase
        if(Retardation) then                                                                                        !   Cyclic phase
            Rp_cycle = (Delta_K0 / Yield)**2 / pi !+ Length_NO_overload                                             !   Cyclic phase Equation 5
            Cp_cycle = (Rp_cycle / (Rp_overload - crack_length_cyclic))**m                                          !   Cyclic phase Equation 2
            C_retardation = Cp_cycle                                                                                !   Cyclic phase
            if(Cp_cycle.ge.One) then                                                                                !   Cyclic phase
                Retardation_Cycles = Cycles - Pre_hydrotest_Cycles                                                  !   Cyclic phase
                Retardation = .false.                                                                               !   Cyclic phase
                C_retardation = One                                                                                 !   Cyclic phase
                !goto 4                                                                                             !   Cyclic phase
            endif                                                                                                   !   Cyclic phase
        else                                                                                                        !   Cyclic phase                                                                                                        !   Cyclic phase
            C_retardation = One                                                                                     !   Cyclic phase
        endif                                                                                                       !   Cyclic phase
    !endif                                                                                                          !   Cyclic phase
    R = K90_min / K90_max                                                                                           !   Cyclic phase
    if(Depth / thickness.gt.0.95D+00) then                                                                          !   Cyclic phase
        Depth = thickness                                                                                           !   Cyclic phase
        TWC = .true.                                                                                                !   Cyclic phase
        goto 5                                                                                                      !   Cyclic phase
    else                                                                                                            !   Cyclic phase
        Delta_K90 = (K90_max - K90_min) * 9.10048D-01                                                               !   Cyclic phase
        dadN_90 = Walker(C_multiplier, m_multiplier, Fatigue_Model, C_retardation, Fatigue_C, Delta_K90, R, m, &
            Fatigue_n)                                                                                              !   Cyclic phase
    endif                                                                                                           !   Cyclic phase
    Delta_K0 = K0_max - K0_min                                                                                      !   Cyclic phase
    R = K0_min / K0_max                                                                                             !   Cyclic phase
    dadN_0 = Walker(C_multiplier, m_multiplier, Fatigue_Model, C_retardation, Fatigue_C, Delta_K0, R, m, Fatigue_n) !   Cyclic phase
    crack_length_cyclic = crack_length_cyclic + dadN_0 * Delta_Cycles                                               !   Cyclic phase
                                                                                                                    !   Cyclic phase
    Delta_K = SurfK0_overload                                                                                       !   Cyclic phase
    dadN_overload = Walker(C_multiplier, m_multiplier, Fatigue_Model, C_retardation, Fatigue_C, Delta_K0, R, m, &   !   Cyclic phase
        Fatigue_n)                                                                                                  !   Cyclic phase
    Delta_K0 = (K0_max - K0_min) * 9.10048D-01                                                                      !   Cyclic phase
    R = K0_min / K0_max                                                                                             !   Cyclic phase
    dadN_0 = Walker(C_multiplier, m_multiplier, Fatigue_Model, C_retardation, Fatigue_C, Delta_K0, Zero, m, &       !   Cyclic phase
        Fatigue_n)                                                                                                  !   Cyclic phase
    if(Delta_Cycles.gt.2) then                                                                                      !   Cyclic phase
        if(dadN_0 * Delta_Cycles / crack_length_cyclic.gt.Max_Delta_Error) then                                     !   Cyclic phase
            Delta_Cycles = Delta_Cycles / 2                                                                         !   Cyclic phase
            Cycles = Hold_Cycles                                                                                    !   Cyclic phase
            goto 3                                                                                                  !   Cyclic phase
        endif                                                                                                       !   Cyclic phase
    endif                                                                                                           !   Cyclic phase
                                                                                                                    !   Cyclic phase
    Depth = Depth + dadN_90 * Delta_Cycles                                                                          !   Cyclic phase
    crack_length_cyclic = crack_length_cyclic + dadN_0 * Delta_Cycles
    Hold_Cycles = Cycles                                                                                            !   Cyclic phase
    
    if(Sample_number.le.50) then
        write(15, '(*(G0.7,:,","))') Cycles, K90_min, K90_max, K0_min, K0_max, &                                    !   Pre-crack phase
            Delta_K90, Delta_K0, Depth, crack_length_cyclic                                                         !   Pre-crack phase
    endif
                                                                                                                    !   Cyclic phase
    J_Normalized = dexp((Delta_K - 12.839D+00) / 3.3644)    
    J_Normalized = 0.012D+00 * dexp(0.3241D+00 * Delta_K)
                                                                                                                    !   Cyclic phase
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
                                                                                                                    !   Cyclic phase
	if(Cycles.le.Cyclic_cycles) then
        goto 3
    endif
4   continue
    Cycle_Range = min(Maximum_cycles, Cycles)
    do I = 1, Cycle_Range
        Print_Cycles(I) = I
        Print_length(1, I) = crack_outputs(I, 5)
        Print_length(2, I) = crack_outputs(I, 6)
    enddo
5   continue
    write(16, '(*(G0.7,:,","))') Sample_number, Retardation_Cycles, Depth_in, Length_in, Fatigue_C, Fatigue_n, Pressure_Cyclic, Pressure_Hydrotest, &
        Delta_K, J_Normalized
    if(Sample_number.eq.1.or.int(Sample_number / 100) * 100.eq. Sample_number) then
        !write(*, 700) Sample_number, Retardation_Cycles
        write(*, 709)
    endif
    
700 format(1x,'Sample: ',I6,' Retardation cycles: ',I7)
709 format(' Epistemic     cot   ','     Logsecant       Modified Logsecant        Emc FE            CorLAS      API      ')
end subroutine Willenborg
    