double precision function Random_Real(Next_Random_Number)
	use mtmod
	implicit none
	!
	!  This function returns a pseudo-random number for each invocation.
	!  It is a FORTRAN 77 adaptation of the "Integer Version 2" minimal 
	!  standard number generator whose Pascal code appears in the article:
	!
	!     Park, Steven K. and Miller, Keith W., "Random Number Generators: 
	!     Good Ones are Hard to Find", Communications of the ACM, 
	!     October, 1988.
	!
	integer*4								::	MPLIER
	integer*4								::	MODLUS
	integer*4								::	MOBYMP
	integer*4								::	MOMDMP
	integer*4								::	HVLUE
	integer*4								::	LVLUE
	integer*4								::	TESTV
	integer*4								::	Next_Random_Number							! Carries the integer for the pseudo-random number generator
	integer*4								::	Fixed_Seed
	integer*4								::	IFRST
		
	COMMON  /Common_Seed/ Fixed_Seed, IFRST
	!SAVE    Next_Random_Number

	intrinsic random_seed, random_number
	integer size 
	integer, allocatable :: seed(:), gseed(:), hiseed(:), zseed(:)
	real harvest(10)
	real*8 x, Mersenne_Twister

	call random_seed(SIZE=size)

	allocate(seed(size),gseed(size),hiseed(size),zseed(size))

	MPLIER=16807
	MODLUS=2147483647
	MOBYMP=127773
	MOMDMP=2836
	!
	data IFRST / 0 /
	!
	
	IF (Fixed_Seed.ne.0.and.IFRST.eq.0) THEN
		Next_Random_Number = Fixed_Seed
		call sgrnd(Fixed_Seed)
		IFRST = 1
	ENDIF
	!

	call random_seed(GET=gseed(1:2))

	Next_Random_Number = gseed(1)
	call random_number(Random_Real)
	
	Random_Real = Mersenne_Twister()

	return

	HVLUE = Next_Random_Number / MOBYMP
	LVLUE = MOD(Next_Random_Number, MOBYMP)
	TESTV = MPLIER*LVLUE - MOMDMP*HVLUE
	IF (TESTV .GT. 0) THEN
		Next_Random_Number = TESTV
	ELSE
		Next_Random_Number = TESTV + MODLUS
	ENDIF
	Random_Real = dfloat(Next_Random_Number)/dfloat(MODLUS)
	!
	RETURN
END

subroutine Start_Random(ISEED)
implicit none
!
!  This subroutine sets the integer seed to be used with the
!  companion drand function to the value of ISEED.  A flag is 
!  set to indicate that the sequence of pseudo-random numbers 
!  for the specified seed should start from the beginning.
!
integer*4								::	Fixed_Seed
integer*4								::	IFRST

integer*4								::	ISeed

COMMON /Common_Seed/Fixed_Seed,IFRST
!

Fixed_Seed = ISEED
IFRST = 0
!
RETURN
END
