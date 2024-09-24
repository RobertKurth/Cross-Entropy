!
  !     ******************************************************************
  subroutine Pre_Programmed_Growth(Transient, Stress_range, Stress_intensity_Range, Cycles_for_Transient, Transient_Temperature, Fatigue_Model_Local, Stress_Intensity_Minimum, Stress_Intensity_Maximum, &
	Time_Scale, JIc, Youngs_modulus, Temperature, Fatigue_Threshold, Rise_time, FCG_Coefficient, FCG_exponent, &
	!Output
	Delta_Growth, dadN)
  !     ******************************************************************
  !        ACTIVATE MECHANISM SPECIFIC FATIGUE CRACK GROWTH
  !     ******************************************************************
	use Program_Constants
    implicit none  
  !
    integer, intent(IN)															::	Cycles_for_Transient
    real*8, intent(IN)															::	JIc
    real*8, intent(IN)															::	Youngs_modulus
    real*8, intent(IN)															::	Stress_Intensity_Maximum
    real*8, intent(IN)															::	Stress_Intensity_Minimum
    real*8, intent(IN)															::	Temperature
    real*8, intent(IN)															::	Transient_Temperature
    real*8, intent(IN)															::	Time_Scale													!Number of time steps per year
    real*8, intent(IN)															::	Fatigue_Threshold
    real*8, intent(IN)															::	FCG_Coefficient
    real*8, intent(IN)															::	FCG_exponent
    real*8, intent(INOUT)                                                       ::	Rise_time
    integer, intent(IN)															::	Fatigue_Model_Local
	
    logical, intent(IN)															::	Stress_range
    real*8, intent(OUT)												            ::	Delta_Growth
    !Local real values
    real*8																		::	Coefficient
    real*8																		::	ZPAR
    real*8																		::	Mean_Value
    real*8																		::	Standard_deviation_value
    real*8																		::	Stress_intensity_Range
    real*8																		::	Stress_intensity_Range_ksi
    real*8																		::	dadN
    real*8																		::	R_ratio
    real*8																		::	Transient_Temperature_F
	real*8																		::	Adjustment_Factor_F
	real*8																		::	K_Knee
	real*8																		::	K_Critical
	real*8																		::	FUNCR
	real*8																		::	FUNCT
	real*8																		::	SIFKNE
	real*8																		::	S_env
	real*8																		::	S_r
	real*8																		::	S_Heckmann
	
	real*8 delt, Ncyc, tau, Ckth, &									!General inputs
            Tmin, Tmax,  &												!Non-custom inputs
            Cni, nni, fniweld, Aenv, nenv, &  		!Ni model inputs
            Css, nss, & 													!SS model inputs
            C1, C2, C3, Ckb, a, S, dadtPWSCC, & 	!Ferritic model inputs
            Ccstm, pcstm, acstm, bcstm, mcstm, &	!Custom model inputs
            dadtFatigue														!outputs
            
	
	integer		Itemp,  &															!Non-custom inputs
            Itip, & 															!Ferritic model inputs
            Irerun, &  														!outputs
            Ierror, &															!inout
						Transient
	!
  !     ******************************************************************
  !          FATIGUE CRACK GROWTH
  !     ******************************************************************
  !
  !     ******************************************************************
  !
  !      (AUSTENITIC STAINLESS STEELS)
  !	
  !     ******************************************************************
  !
    Transient_Temperature_F = 1.8D+00 * Transient_Temperature + 32.0D+00
    K_Critical = dsqrt(JIc * Youngs_modulus /1000.0D+00)

! Fatigue_Model   Model
!--------------   -----------------------------------------------------------------------------------------------
! 1               Paris
! 2               Forman with K_Critical obtained from material at crack 
! 3               Walker with 1 - m set to 0.5
! 4               BWR PLR from "Benchmark analysis on PFM analysis codes for aged piping of nuclear power plants"  
! 5               Fatigue NUREG 6986 da/dN
! 6               Fatigue NUREG 6674 da/dN
! 7               Fatigue PROLOCA 2005
! 8             			Fatigue Nickel based
! 9								Fatigue Heckmann GRS
	
    if (Fatigue_Model_Local.EQ.1) then
		if(Stress_range) then
		else
			Stress_intensity_Range = dmax1(0.0D+00, (Stress_Intensity_Maximum - Stress_Intensity_Minimum))
		endif
		if(Stress_intensity_Range.GT.Fatigue_Threshold) then                                                            ! Paris fatigue crack growth from User Input
			dadN = FCG_Coefficient*(Stress_intensity_Range)**FCG_exponent                                                      ! Paris Fatigue crack growth from User Input
		else                                                                                                            ! Paris Fatigue crack growth from User Input
			dadN = 0.0D0                                                                                                ! Paris Fatigue crack growth from User Input
		endif                                                                                                           ! Paris Fatigue crack growth from User Input
		Delta_Growth = dadN * Cycles_for_Transient * 1000.0D+00                                                         ! Paris Fatigue crack growth from User Input
		IF(Delta_Growth.lt.0.0D0) THEN                                                                                  ! Paris Fatigue crack growth from User Input
			Delta_Growth = 0.0D0                                                                                        ! Paris Fatigue crack growth from User Input
		endif                                                                                                           ! Paris Fatigue crack growth from User Input
		return
    endif
  !
	if (Fatigue_Model_Local.EQ.2) then
		if(Stress_Intensity_Maximum.gt.0) then																			! Forman Fatigue crack growth
			R_ratio = Stress_Intensity_Minimum / Stress_Intensity_Maximum												! Forman Fatigue crack growth
		else																											! Forman Fatigue crack growth
			R_ratio = 0.0																								! Forman Fatigue crack growth
		endif																											! Forman Fatigue crack growth
		Stress_intensity_Range = dmax1(0.0D+00, Stress_Intensity_Maximum-Stress_Intensity_Minimum)                      ! Forman Fatigue crack growth

		!
		!	NURBIM Limit
		!
		
		if(Stress_intensity_Range.GT.Fatigue_Threshold) then															! Forman Fatigue crack growth
			dadN = FCG_Coefficient * (Stress_intensity_Range ** FCG_exponent) / (1.0D+00 - R_ratio) / &                        ! Forman Fatigue crack growth
			(K_Critical - Stress_Intensity_Maximum)																		! Forman Fatigue crack growth
		else																											! Forman Fatigue crack growth
			dadN = 0.0D0																								! Forman Fatigue crack growth
		endif																											! Forman Fatigue crack growth
		Delta_Growth = dadN * Cycles_for_Transient																		! Forman Fatigue crack growth
	endif
	!
	!	NURBIM Limit
	!
	Delta_Growth = dmax1(0.0D-00, Delta_Growth)
  !
  if (Fatigue_Model_Local.EQ.3) then
      if(Stress_Intensity_Maximum.gt.0) then                                                                            ! Walker Fatigue crack growth
          R_ratio = Stress_Intensity_Minimum / Stress_Intensity_Maximum                                                 ! Walker Fatigue crack growth
      else                                                                                                              ! Walker Fatigue crack growth
          R_ratio = 0.0                                                                                                 ! Walker Fatigue crack growth
      endif                                                                                                             ! Walker Fatigue crack growth
      Stress_intensity_Range = dmax1(0.0D+00, Stress_Intensity_Maximum-Stress_Intensity_Minimum)                        ! Walker Fatigue crack growth
      if(Stress_intensity_Range.GT.Fatigue_Threshold) then                                                              ! Walker Fatigue crack growth
          dadN = FCG_Coefficient * (Stress_intensity_Range / dsqrt(1.0D+00 - R_ratio)) ** FCG_exponent                         ! Walker Fatigue crack growth
      else                                                                                                              ! Walker Fatigue crack growth
          dadN = 0.0D0                                                                                                  ! Walker Fatigue crack growth
      endif                                                                                                             ! Walker Fatigue crack growth
      Delta_Growth = dadN*Cycles_for_Transient																			! Walker Fatigue crack growth
  endif
  !
  if(Fatigue_Model_Local.eq.4) then                                                                                     ! Fatigue crack growth for BWR PLR
    !                                                                                                                   ! Fatigue crack growth for BWR PLR
    ! Equation 3 from "Benchmark analysis on PFM analysis codes for aged piping of nuclear power plants"                ! Fatigue crack growth for BWR PLR
    ! Horoto Itoh, Yinsheng Li, Kazuya Osakabe, Kunio Onizawa, and Shinobu Yoshimura                                    ! Fatigue crack growth for BWR PLR
    !                                                                                                                   ! Fatigue crack growth for BWR PLR
    R_ratio = Stress_Intensity_Minimum / Stress_Intensity_Maximum                                                       ! Fatigue crack growth for BWR PLR
    Stress_intensity_Range = dmax1(0.0D+00, Stress_Intensity_Maximum - Stress_Intensity_Minimum)
    !                                                                                                                   ! Fatigue crack growth for BWR PLR
    !                                                                                                                   ! Fatigue crack growth for BWR PLR
    !                                                                                                                   ! Fatigue crack growth for BWR PLR
    if(Rise_Time.lt.10.0D+00) then																					! Fatigue crack growth for BWR PLR
      Rise_Time =dsqrt(1000.0D+00)                                                                                      ! Fatigue crack growth for BWR PLR
    else                                                                                                                ! Fatigue crack growth for BWR PLR
      Rise_Time =dsqrt(Rise_Time)																					! Fatigue crack growth for BWR PLR
    endif                                                                                                               ! Fatigue crack growth for BWR PLR
                                                                                                                        ! Fatigue crack growth for BWR PLR        
    if(Stress_intensity_Range.GT.Fatigue_Threshold) then                                                                ! Fatigue crack growth for BWR PLR
      dadN = FCG_Coefficient * Rise_Time * (Stress_intensity_Range ** 3) / ((1.0D+00 - R_ratio) ** 2.12)                ! Fatigue crack growth for BWR PLR
    else                                                                                                                ! Fatigue crack growth for BWR PLR
      dadN = 0.0D0                                                                                                      ! Fatigue crack growth for BWR PLR
    endif                                                                                                               ! Fatigue crack growth for BWR PLR
    Delta_Growth = dadN*Cycles_for_Transient																			! Fatigue crack growth for BWR PLR
    return                                                                                                              ! Fatigue crack growth for BWR PLR
  endif
    
  if(Fatigue_Model_Local.eq.5) then                                                                                 ! Fatigue crack growth from NUREG 6986
    !                                                                                                               ! Fatigue crack growth from NUREG 6986
    ! Equation 4.2 from NUREG 6986                                                                                  ! Fatigue crack growth from NUREG 6986
    !                                                                                                               ! Fatigue crack growth from NUREG 6986
    if(Stress_Intensity_Maximum.gt.0) then                                                                          ! Fatigue crack growth from NUREG 6986
        R_ratio = Stress_Intensity_Minimum / Stress_Intensity_Maximum                                               ! Fatigue crack growth from NUREG 6986
    else                                                                                                            ! Fatigue crack growth from NUREG 6986
        R_ratio = 0.0                                                                                               ! Fatigue crack growth from NUREG 6986
        endif                                                                                                       ! Fatigue crack growth from NUREG 6986
    Stress_intensity_Range = dmax1(0.0D+00, Stress_Intensity_Maximum-Stress_Intensity_Minimum)
    if(Stress_intensity_Range.GT.Fatigue_Threshold) then                                                            ! Fatigue crack growth from NUREG 6986
      dadN = FCG_Coefficient * (Stress_intensity_Range / dsqrt(1.0D+00 - R_ratio)) ** FCG_exponent                  ! Fatigue crack growth from NUREG 6986
    else                                                                                                            ! Fatigue crack growth from NUREG 6986
      dadN = 0.0D0                                                                                                  ! Fatigue crack growth from NUREG 6986
    endif                                                                                                           ! Fatigue crack growth from NUREG 6986
    Delta_Growth = dadN*Cycles_for_Transient																																				! Fatigue crack growth from NUREG 6986
    return                                                                                                          ! Fatigue crack growth from NUREG 6986
  endif
  
  if(Fatigue_Model_Local.eq.6) then                                                                                 ! Fatigue crack growth from NUREG 6674
      !                                                                                                             ! Fatigue crack growth from NUREG 6674
      !                   NUREG 6674                                                                                ! Fatigue crack growth from NUREG 6674
      !                                                                                                             ! Fatigue crack growth from NUREG 6674
      Stress_intensity_Range = dmax1(0.0D+00, Stress_Intensity_Maximum-Stress_Intensity_Minimum)
      if(Stress_Intensity_Maximum.gt.0) then																														            ! Fatigue crack growth from NUREG 6674
          R_ratio = Stress_Intensity_Minimum / Stress_Intensity_Maximum                                             ! Fatigue crack growth from NUREG 6674
      else                                                                                                          ! Fatigue crack growth from NUREG 6674
          R_ratio = 0.0                                                                                             ! Fatigue crack growth from NUREG 6674
      endif																																																          ! Fatigue crack growth from NUREG 6674
      
      K_Knee = 17.74D+00
      if(R_ratio.gt.0.25D+00.and.R_ratio.lt.0.65D+00) then
          K_Knee = K_Knee *((3.75D+00*R_ratio + 0.06D+00)/(26.9D+00*R_ratio - 5.725D+00))**0.25D+00
      elseif(R_ratio.ge.0.65D+00) then
          K_Knee = 12.04D+00
      endif
      
      if(Stress_intensity_Range.lt.K_Knee) then
          Adjustment_Factor_F = 1.0D+00
          if(R_ratio.gt.0.25D+00.and.R_ratio.lt.0.65D+00) then
              Adjustment_Factor_F = 26.90D+00*R_ratio - 5.725D+00
          else
              Adjustment_Factor_F = 11.76D+00
          endif
      else
          Adjustment_Factor_F = 1.0D+00
          if(R_ratio.gt.0.25D+00.and.R_ratio.lt.0.65D+00) then
              Adjustment_Factor_F = 3.75D+00*R_ratio - 0.06D+00
          elseif(R_ratio.ge.0.65D+00) then
              Adjustment_Factor_F = 11.76D+00
          endif
      endif
					
    if(Stress_intensity_Range.le.K_Knee) then																																			 ! Fatigue crack growth from NUREG 6674
        Stress_intensity_Range = Stress_intensity_Range                                                            ! Fatigue crack growth from NUREG 6674
        dadN = FCG_Coefficient * Adjustment_Factor_F * 1.03D-12 * Stress_intensity_Range ** 5.95D+00               ! Fatigue crack growth from NUREG 6674
    else																																																					 ! Fatigue crack growth from NUREG 6674
        Stress_intensity_Range = Stress_intensity_Range                                                            ! Fatigue crack growth from NUREG 6674
        dadN = FCG_Coefficient * Adjustment_Factor_F * 1.01D-07 * Stress_intensity_Range ** 1.95D+00               ! Fatigue crack growth from NUREG 6674
		endif																																																					 ! Fatigue crack growth from NUREG 6674
		!
		!	NUREG 6674 da/dN is inches / cycle change to mm / cycle
		!
    Delta_Growth = dadN*Cycles_for_Transient*25.4D+00                                                              ! Fatigue crack growth from NUREG 6674
    return																																																				 ! Fatigue crack growth from NUREG 6674
  endif
    

	if(Fatigue_Model_Local.eq.10) then	
		!														                                                    ! Fatigue crack growth ASME Code Section XI Appendix C - 8411
        ! da/dN = FCG_Coefficient x Sr x Senv x DK^FCG_exponent                             								! Fatigue crack growth ASME Code Section XI Appendix C - 8411
        ! Sr =(1 - 0.82 x R)^-2.2																					! Fatigue crack growth ASME Code Section XI Appendix C - 8411
        ! R = Kmin / Kmax                             																! Fatigue crack growth ASME Code Section XI Appendix C - 8411
		!																											! Fatigue crack growth ASME Code Section XI Appendix C - 8411
		if(Stress_Intensity_Maximum.gt.0) then																		! Fatigue crack growth ASME Code Section XI Appendix C - 8411
            R_ratio = Stress_Intensity_Minimum / Stress_Intensity_Maximum											! Fatigue crack growth ASME Code Section XI Appendix C - 8411
        else								                                                                        ! Fatigue crack growth ASME Code Section XI Appendix C - 8411
            R_ratio = 0.0																							! Fatigue crack growth ASME Code Section XI Appendix C - 8411
		endif																										! Fatigue crack growth ASME Code Section XI Appendix C - 8411
																													! Fatigue crack growth ASME Code Section XI Appendix C - 8411
		Stress_intensity_Range = dmax1(0.0D+00, Stress_Intensity_Maximum - Stress_Intensity_Minimum)	            ! Fatigue crack growth ASME Code Section XI Appendix C - 8411
        if(Stress_intensity_Range.GT.Fatigue_Threshold) then														! Fatigue crack growth ASME Code Section XI Appendix C - 8411
          S_r = (1.0D+00 - 0.82D+00 * R_ratio) ** (-2.2D+00)														! Fatigue crack growth ASME Code Section XI Appendix C - 8411
          S_env = 1.0D+00																							! Fatigue crack growth ASME Code Section XI Appendix C - 8411
          dadN = FCG_Coefficient * S_r * Stress_intensity_Range ** FCG_exponent                                     ! Fatigue crack growth ASME Code Section XI Appendix C - 8411
		else																										! Fatigue crack growth ASME Code Section XI Appendix C - 8411
		    dadN = 0.0D0																							! Fatigue crack growth ASME Code Section XI Appendix C - 8411
		endif																										! Fatigue crack growth ASME Code Section XI Appendix C - 8411    
		Delta_Growth = dadN*Cycles_for_Transient																	! Fatigue crack growth ASME Code Section XI Appendix C - 8411
    return																											! Fatigue crack growth ASME Code Section XI Appendix C - 8411
  endif
  	
end subroutine Pre_Programmed_Growth
	