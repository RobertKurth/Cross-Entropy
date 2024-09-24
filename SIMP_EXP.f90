!
	DOUBLE PRECISION FUNCTION SIMP_EXP(A, B, AM)
    implicit none
    real*8 A, B, AM, cmom, XX, XL, XR, Exponential_Moment
    real*8 DIV, ERROR, FIXED, HALF, MLIMIT, NPAN, Save_X, SIMOLD, SUM
    integer I, J_index, K
    !
    ERROR = 0.00000001D+00
    NPAN = 2
    SIMP_EXP = Exponential_Moment(A, AM) + Exponential_Moment(B, AM)
    XX = 0.5D+00*(A + B)
    
    DIV = 0.5D+00*(B-A)
    
    !	DIV = (B-A)
    
    FIXED = SIMP_EXP
    HALF = Exponential_Moment(XX, AM)
    SIMP_EXP = SIMP_EXP + 4.0D+00*HALF
    SIMP_EXP = SIMP_EXP*DIV/3.0D+00
    FIXED = FIXED+2.0D+00*HALF
1   SIMOLD = SIMP_EXP
    SUM = 0.0D+00
    MLIMIT = NPAN/2
    NPAN = NPAN*2
    DO 2 I = 1, MLIMIT
        J_index = NPAN-2*I + 1
        K = 2*I-1
        XL = (J_index*A + K*B)/NPAN
        XR = (K*A + J_index*B)/NPAN
        SUM = SUM + (Exponential_Moment(XL, AM) + Exponential_Moment(XR, AM))
2   CONTINUE
    SAVE_X = SUM
    SUM = 4.0D+00*SUM + FIXED
    FIXED = FIXED+2.0D+00*SAVE_X
    DIV = (B-A)/NPAN
    SIMP_EXP = DIV*SUM/3.0D+00
33  FORMAT(' NPAN, SIMP, SIMOLD ', I8, 2(1X, 1PE12.5))
    IF(NPAN.GT.5500) GOTO 1010
    IF(SIMP_EXP.EQ.0) GOTO 1
    IF(DABS((SIMP_EXP-SIMOLD)/SIMP_EXP).GT.ERROR) GOTO 1
    IF(NPAN.LT.128) GOTO 1
342 FORMAT(' SIMP = ', 1PE12.5)
1010 RETURN
END

DOUBLE PRECISION FUNCTION Exponential_Moment(x, Lambda)
implicit none
real*8 x, Lambda
Exponential_Moment = x * Lambda * dexp(-Lambda * x)
return
end