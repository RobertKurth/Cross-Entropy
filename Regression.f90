subroutine Regression(Next_Random_Number, Number_of_variables, Number_of_data, Polynomial, Number_Parameters, &
        Measured_Data, Updated_Data, Beta, Theta_lower, Theta, Theta_upper, Case_Filename, Number_of_bins)
	use Program_Constants
	USE MSFLIB
	USE IFLOGM
	implicit none
	INTERFACE 
	LOGICAL(4) FUNCTION INITIALSETTINGS
	END FUNCTION
	END INTERFACE
	integer, intent(IN)                                                     :: Next_Random_Number
	integer, intent(IN)                                                     :: Number_of_variables
	integer, intent(IN)                                                     :: Number_of_data
	integer, intent(IN)                                                     :: Polynomial
	integer, intent(IN)										                :: Number_Parameters
	integer, intent(IN)										                :: Number_of_bins
    integer													                :: Epistemic
	integer													                :: Aleatory

	real*8, intent(OUT), dimension(1:Number_Parameters)						:: Beta
	real*8, dimension(1:Number_Parameters), intent(INOUT)					:: Theta_lower
	real*8, dimension(1:Number_Parameters), intent(INOUT)					:: Theta
	real*8, dimension(1:Number_Parameters), intent(INOUT)					:: Theta_upper

    real*8, intent(IN), dimension(1:Number_of_Data, 0:Number_of_Variables)	:: Measured_Data
	real*8, intent(IN), dimension(1:Number_of_Data, 0:Number_of_Variables)	:: Updated_Data
	real*8, dimension(1:Number_of_data, 0:Number_of_variables)				:: x_mean
    real*8, dimension(1:Number_of_data)                                     :: y_measured
	!
	! equation (3.28) of reference [1]
	!
	! Number_of_variables: Number_of_variables
	! Number_of_data: Number_of_data
	!
	real*8, dimension(1:Number_of_Variables, 1:2, 1:Number_of_bins)							            :: DPD		! DPD before adaptive clustering
	real*8, dimension(1:Number_of_data, 0:Number_of_variables)				:: x
	real*8, dimension(1:Number_Parameters)						:: v
	real*8, dimension(0:(Number_of_variables), 1:7)			            :: Parameters
	real*8, dimension(1:Number_of_data)										:: y
	real*8, dimension(1:Number_of_data, 1:Number_Parameters)	:: Z
	real*8, dimension(1:Number_Parameters, 1:Number_of_data)	:: Z_transpose

	real*8, dimension(1:Number_Parameters, 1:Number_Parameters)                       :: Z_inverse
	real*8, dimension(1:Number_Parameters, 1:Number_Parameters)                       :: A
	real*8, dimension(1:Number_Parameters, 1:Number_Parameters)                       :: Lower
	real*8, dimension(1:Number_Parameters, 1:Number_Parameters)                       :: L_Inverse
	real*8, dimension(1:Number_Parameters, 1:Number_Parameters)                       :: Upper
	real*8, dimension(1:Number_Parameters, 1:Number_Parameters)                       :: U_Inverse
    
	integer														            :: I
	integer                                                                 :: J
	integer                                                                 :: K
	integer                                                                 :: L
	integer                                                                 :: Iv
	integer                                                                 :: Unit_Number

	integer                                                                 :: K_Epistemic
	integer                                                                 :: K_Aleatory
	integer                                                                 :: Number_of_distribution_parameters = 7
		
	integer                                                                 :: Distribution_Type
	integer													                :: Number_DPD_Variables = 1

	integer                                                                 :: IS
	real*8													                :: Mean_Value
	real*8													                :: Standard_Deviation
	real*8													                :: Weibull_Scale = Zero
	real*8													                :: Weibull_Beta = Zero
	real*8													                :: X_Lower
	real*8													                :: X_Upper
	real*8													                :: Sample	
	
	real*8													                :: Time_output
	real*8													                :: Time_start
	real*8													                :: Time_end
	real*8													                :: Time_per_simulation
	
	CHARACTER*256                                                            :: Input_file
	CHARACTER*256                                                            :: Output_file
	CHARACTER*256, dimension(0:Number_of_variables)                                        :: Variable_Names
	CHARACTER*256, dimension(1:Number_of_data)                                            :: Pipe_test
	CHARACTER*256, dimension(1:18)                                           :: Column_titles
	CHARACTER*256, dimension(1:Number_of_data)                                            :: Manufacturer
	CHARACTER*256, dimension(1:Number_of_data)                                            :: Location
	CHARACTER*256, dimension(1:Number_of_data)                                            :: Seam_type
	CHARACTER*256, dimension(1:Number_of_data)                                            :: Brittle_Initiation
	CHARACTER*256, dimension(1:Number_of_data)                                            :: Grade
	real*8, dimension(1:Number_of_data)										            :: Sample_ID
	real*8, dimension(1:Number_of_data)										            :: Install_Year

	integer*4								                                :: Hundredths_seconds, Year, Day_add, Hour_add, Minute_add, Seconds_add

	integer									                                :: Month
	integer									                                :: Day
	integer									                                :: Hour
	integer									                                :: Minute
	integer									                                :: Seconds
	integer									                                :: Current_Simulation
	
	character*1								                                :: Month_1
	character*1								                                :: Day_1
	character*1								                                :: Hour_1
	character*1								                                :: Minute_1
	character*1								                                :: Seconds_1
	
	character*1								                                :: Month_2
	character*1								                                :: Day_2
	character*1								                                :: Hour_2
	character*1								                                :: Minute_2
	character*1								                                :: Seconds_2
	character*2								                                :: am_pm
	
    CHARACTER*256															:: Case_Filename
	real*8									                                :: Current_total_time
	real*8									                                :: Finish_time
	character*1, dimension(1:11)                                            :: Number_character
    
	real*8, dimension(1:Number_Parameters)									:: Relative_Variance

	real*8														            :: Calculated_CVP_FSE
    real*8														            :: Random_Real
    integer														            :: Sampled_bin

	DATA Number_character /'0','1','2','3','4','5','6','7','8','9','0'/

    x_mean(1:Number_of_Data, 1:Number_of_Variables) = Measured_Data(1:Number_of_Data, 1:Number_of_Variables)
	y_measured(1:Number_of_Data) = Measured_Data(1:Number_of_Data, 0)
    Unit_Number = 1
	close(Unit_Number)
	Input_file ='..\input\Distribution.csv'
	call Open_File_Read(Unit_Number, Input_file)
	read(1, *) Aleatory, Epistemic
	do Iv = 0, Number_of_variables
		read(1, *) Variable_Names(Iv), (Parameters(Iv, I), I = 1, Number_of_distribution_parameters)
	enddo
	close(1)
    
	Unit_Number = 3
	close(Unit_Number)
	Output_file ='..\results\Matrices_'//trim(Case_Filename)//'.csv'
	call Open_File_Write(Unit_Number, Output_file)
    
	CALL GETTIM (Hour, Minute, Seconds, Hundredths_seconds)
	CALL GETDAT (Year, Month, Day)

	call CPU_TIME(Time_start)
	Current_Simulation = 0
	do K_Epistemic = 1, Epistemic
		do K_Aleatory = 1, Aleatory
            Current_Simulation = Current_Simulation + 1
			do K = 1, Number_of_data
				do Iv = 0, Number_of_variables
					Distribution_Type = int(Parameters(Iv, 1))
					Mean_Value = x_mean(K, Iv)
					Standard_Deviation = Mean_Value * Parameters(Iv, 3)
					if(Mean_Value.gt.0) then
						X_Lower = Mean_Value *	Parameters(Iv, 4)
						X_Upper = Mean_Value *	Parameters(Iv, 5)
					else
						X_Lower = Mean_Value *	Parameters(Iv, 5)
						X_Upper = Mean_Value *	Parameters(Iv, 4)
                    endif
                    if(Parameters(Iv, Number_of_distribution_parameters).eq.Zero) then
						Sampled_bin = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
						x(K, Iv) = Sample(Next_Random_Number, Number_of_Variables, Iv, Number_of_bins, Variable_Names(Iv), Distribution_Type, Mean_Value, Standard_Deviation, &
							X_Lower, X_Upper, DPD, Sampled_bin)
                    endif
					Distribution_Type = int(Parameters(Iv, 1))
					Mean_Value = y_measured(K)
					Standard_Deviation = Mean_Value * Parameters(Iv, 3)
					X_Lower = Mean_Value *	Parameters(Iv, 4)
					X_Upper = Mean_Value *	Parameters(Iv, 5)
					!y(K) = Sample(Next_Random_Number, Number_of_Variables, Iv, Number_of_bins, Variable_Names(Iv), Distribution_Type, Mean_Value, Standard_Deviation, &
					!	Weibull_Scale, Weibull_Beta, X_Lower, X_Upper, DPD, Sampled_bin)
                    !Sampled_bin = int(Number_of_bins * Random_Real(Next_Random_Number)) + 1
					!y(K) = Calculated_CVP_FSE(K, x, Number_of_data, Number_of_variables)
                    y(K) = y_measured(K)
                enddo
			enddo
				
			if(Polynomial.eq.0) then
                Z(1:Number_of_data, 1:Number_of_variables) = x(1:Number_of_data, 1:Number_of_variables)
			else
				do I = 1, Number_of_data
					L = 1
					Z(I, L) = One
					do J = 1, Number_of_variables
						do K = 1, Polynomial
							L = L + 1
							Z(I, L) = x_mean(I, J)**(K)
						enddo
					enddo
				enddo
			endif
            
			call transpose(Number_Parameters, Number_of_data, Z, Z_transpose)
			call matrix_multiply(Number_Parameters, Number_of_data, Z_transpose, Z, A)
    
			call Factor_Matrix(A, Upper, Lower, Number_Parameters)
			call A_inverse(A, Z_inverse, Lower, L_Inverse, Upper, U_Inverse, Number_Parameters)

			call matrix_vector(Number_Parameters, Number_of_data, Z_transpose, y, v) 
			call matrix_vector(Number_Parameters, Number_Parameters, Z_inverse, v, Beta)
            
            write(3,'(*(G0.7,:,","))')'Z'
			do I = 1, Number_of_data
                write(3,'(*(G0.7,:,","))') (Z(I, J), J = 1, Number_Parameters)
            enddo
            
            write(3,'(*(G0.7,:,","))')'Z_transpose'        
			do I = 1, Number_Parameters
                write(3,'(*(G0.7,:,","))') (Z_transpose(I, J), J = 1, Number_of_data)
            enddo
            
            write(3,'(*(G0.7,:,","))')'A'        
			do I = 1, Number_Parameters
                write(3,'(*(G0.7,:,","))') (A(I, J), J = 1, Number_Parameters)
            enddo
            
            write(3,'(*(G0.7,:,","))')'Lower'        
			do I = 1, Number_Parameters
                write(3,'(*(G0.7,:,","))') (Lower(I, J), J = 1, Number_Parameters)
            enddo
            
            write(3,'(*(G0.7,:,","))') "L_Inverse "
            do I = 1, Number_Parameters
                write(3,'(*(G0.7,:,","))') (L_Inverse(I, J), J = 1, Number_Parameters)
            enddo
            
            write(3,'(*(G0.7,:,","))')'Upper'
            do I = 1, Number_Parameters
                write(3,'(*(G0.7,:,","))') (Upper(I, J), J = 1, Number_Parameters)
            enddo
            
            write(3,'(*(G0.7,:,","))')'U_Inverse'
            do I = 1, Number_Parameters
                write(3,'(*(G0.7,:,","))') (U_Inverse(I, J), J = 1, Number_Parameters)
            enddo
            
            write(3,'(*(G0.7,:,","))')'Z_inverse'
            do I = 1, Number_Parameters
                write(3,'(*(G0.7,:,","))') (Z_inverse(I, J), J = 1, Number_Parameters)
            enddo
            
            write(11,'(*(G0.7,:,","))') Case_Filename
            do I = 1, Number_Parameters
                write(11,'(*(G0.7,:,","))') Beta(I)
            enddo
            
            write(12,'(*(G0.7,:,","))') Case_Filename
			write(12,'(*(G0.7,:,","))') (Beta(I), I = 1, Number_Parameters)
			do I = 1, Number_of_data
				if(Polynomial.eq.1) then
					write(12,'(*(G0.7,:,","))') K_Epistemic, K_Aleatory, (x(I, J), J = 1, Number_of_variables), y(I)
				elseif(Polynomial.eq.2) then
					write(12,'(*(G0.7,:,","))') K_Epistemic, K_Aleatory, (x(I, J), x(I, J)**2, J = 1, Number_of_variables), y(I)
				elseif(Polynomial.eq.3) then
					write(12,'(*(G0.7,:,","))') K_Epistemic, K_Aleatory, (x(I, J), x(I, J)**2, x(I, J)**3, J = 1, Number_of_variables), y(I)
				elseif(Polynomial.eq.4) then
					write(12,'(*(G0.7,:,","))') K_Epistemic, K_Aleatory, (x(I, J), x(I, J)**2, x(I, J)**3, x(I, J)**4, J = 1, Number_of_variables), y(I)
				elseif(Polynomial.eq.5) then
					write(12,'(*(G0.7,:,","))') K_Epistemic, K_Aleatory, (x(I, J), x(I, J)**2, x(I, J)**3, x(I, J)**4, x(I, J)**5, J = 1, Number_of_variables), y(I)
				endif
			enddo
    
            
            close(3)
                    
        enddo
	
		if(int(Epistemic / 4) * 4.eq.Epistemic) then
			call CPU_TIME(Time_end)
			!Time_per_simulation = (Time_end - Time_start) * dfloat(Epistemic) / dfloat(K_Epistemic)
            Time_per_simulation = Time_end * dfloat(Epistemic) / dfloat(K_Epistemic)
			write(*, *) K_Epistemic, K_Aleatory - 1

			Current_total_time = Time_End / dfloat(K_Epistemic * Aleatory) * dfloat(Epistemic * Aleatory)
		
			Finish_time = Current_total_time * dfloat(Epistemic * Aleatory - K_Epistemic * Aleatory)/dfloat(Epistemic * Aleatory)
		
			call Time_Estimate(Year, Month, Day, Finish_time, Current_total_time, Day_1, Day_2, Hour_1, Hour_2, Minute_1, Minute_2, Seconds_1, Seconds_2)
			write(*, 700)	Day_1, Day_2, Hour_1, Hour_2, Minute_1, Minute_2, Seconds_1, Seconds_2
		endif
	enddo

    Finish_time = (Time_end - Time_start)
    call Time_Estimate(Year, Month, Day, Finish_time, Current_total_time, Day_1, Day_2, Hour_1, Hour_2, Minute_1, Minute_2, Seconds_1, Seconds_2)

    write(*, 701)	Day_1, Day_2, Hour_1, Hour_2, Minute_1, Minute_2, Seconds_1, Seconds_2
    
700 format(1x,'	estimated finish time:',2A1,' days',2A1,' hours',2A1,' minutes',2A1,' seconds',2A1)
701 format(1x,'	Total time:',2A1,' days',2A1,' hours',2A1,' minutes',2A1,' seconds',2A1)
    
end subroutine Regression
