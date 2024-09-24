!
	DOUBLE PRECISION FUNCTION SIMP_PDF_EXP(A, B, Lambda)
    implicit none
    real*8 A, B, Lambda
    real*8 CMOM_PDF, DIV, ERROR, FIXED, HALF, SAVE_X, SIMOLD, SUM, XL, XR, XX
    integer I, J_index, K, MLIMIT, NPAN
    !
    ERROR = 0.00000001D+00
    NPAN = 2
    SIMP_PDF_EXP = Lambda * dexp(-Lambda * A) + Lambda * dexp(-Lambda * B)
    XX = 0.5D+00*(A + B)
    
    DIV = 0.5D+00*(B-A)
    
    DIV = (B-A)
    
    FIXED = SIMP_PDF_EXP
    HALF = Lambda * dexp(-Lambda * XX)
    SIMP_PDF_EXP = SIMP_PDF_EXP + 4.0D+00*HALF
    SIMP_PDF_EXP = SIMP_PDF_EXP*DIV/3.0D+00
    FIXED = FIXED+2.0D+00*HALF
1   SIMOLD = SIMP_PDF_EXP
    SUM = 0.0D+00
    MLIMIT = NPAN/2
    NPAN = NPAN*2
    DO I = 1, MLIMIT
        J_index = NPAN-2*I + 1
        K = 2*I-1
        XL = (J_index*A + K*B)/NPAN
        XR = (K*A + J_index*B)/NPAN
        SUM = SUM + (Lambda * dexp(-Lambda * XL) + Lambda * dexp(-Lambda * XR))
    enddo
    SAVE_X = SUM
    SUM = 4.0D+00*SUM + FIXED
    FIXED = FIXED+2.0D+00*SAVE_X
    DIV = (B-A)/NPAN
    SIMP_PDF_EXP = DIV*SUM/3.0D+00

    IF(NPAN.GT.5500) GOTO 2
    IF(SIMP_PDF_EXP.EQ.0) GOTO 1
    IF(DABS((SIMP_PDF_EXP-SIMOLD)/SIMP_PDF_EXP).GT.ERROR) GOTO 1
    IF(NPAN.LT.128) GOTO 1
2   RETURN
END
