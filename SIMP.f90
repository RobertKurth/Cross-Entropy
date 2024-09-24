!
	DOUBLE PRECISION FUNCTION SIMP(A, B, AM, SD)
  implicit none
  real*8 A, B, AM, SD, cmom, XX, XL, XR
  real*8 DIV, ERROR, FIXED, HALF, MLIMIT, NPAN, Save_Simp, SIMOLD, SUM
  integer I, J_index, K
  ERROR = 0.000001
  NPAN = 2
  SIMP = CMOM(A, AM, SD) + CMOM(B, AM, SD)
  XX = 0.5*(A + B)

  DIV = 0.5*(B-A)

  DIV = (B-A)

  FIXED = SIMP
  HALF = CMOM(XX, AM, SD)
  SIMP = SIMP + 4.0*HALF
  SIMP = SIMP*DIV/3.0
  FIXED = FIXED+2.0*HALF
1  SIMOLD = SIMP
  SUM = 0.0
  MLIMIT = NPAN/2
  NPAN = NPAN*2
  DO 2 I = 1, MLIMIT
  J_index = NPAN-2*I + 1
  K = 2*I-1
  XL = (J_index*A + K*B)/NPAN
  XR = (K*A + J_index*B)/NPAN
  SUM = SUM + (CMOM(XL, AM, SD) + CMOM(XR, AM, SD))
2  CONTINUE
  Save_Simp = SUM
  SUM = 4.0*SUM + FIXED
  FIXED = FIXED+2.0*Save_Simp
  DIV = (B-A)/NPAN
  SIMP = DIV*SUM/3.0
33  FORMAT(' NPAN, SIMP, SIMOLD ', I8, 2(1X, 1PE12.5))
  IF(NPAN.GT.5500) GOTO 1010
  IF(SIMP.EQ.0) GOTO 1
  IF(DABS((SIMP-SIMOLD)/SIMP).GT.ERROR) GOTO 1
  IF(NPAN.LT.64) GOTO 1
342  FORMAT(' SIMP = ', 1PE12.5)
1010  RETURN
  END
