DOUBLE PRECISION FUNCTION SIMP_PDF(A, B, AM, SD)
    implicit none
    real*8 A, B, AM, SD
    real*8 Normal_PDF, DIV, ERROR, FIXED, HALF, SAVE_X, SIMOLD, SUM, XL, XR, XX
    integer I, J_index, K, MLIMIT, NPAN
    !
    ERROR = 0.00000001D+00
    NPAN = 2
    SIMP_PDF = Normal_PDF(A, AM, SD) + Normal_PDF(B, AM, SD)
    XX = 0.5D+00*(A + B)
    
    DIV = 0.5D+00*(B-A)
    
    DIV = (B-A)
    
    FIXED = SIMP_PDF
    HALF = Normal_PDF(XX, AM, SD)
    SIMP_PDF = SIMP_PDF + 4.0D+00*HALF
    SIMP_PDF = SIMP_PDF*DIV/3.0D+00
    FIXED = FIXED+2.0D+00*HALF
1   SIMOLD = SIMP_PDF
    SUM = 0.0D+00
    MLIMIT = NPAN/2
    NPAN = NPAN*2
    DO I = 1, MLIMIT
        J_index = NPAN-2*I + 1
        K = 2*I-1
        XL = (J_index*A + K*B)/NPAN
        XR = (K*A + J_index*B)/NPAN
        SUM = SUM + (Normal_PDF(XL, AM, SD) + Normal_PDF(XR, AM, SD))
    enddo
    SAVE_X = SUM
    SUM = 4.0D+00*SUM + FIXED
    FIXED = FIXED+2.0D+00*SAVE_X
    DIV = (B-A)/NPAN
    SIMP_PDF = DIV*SUM/3.0D+00
    
    IF(NPAN.GT.5500) GOTO 2
    IF(SIMP_PDF.EQ.0) GOTO 1
    IF(DABS((SIMP_PDF-SIMOLD)/SIMP_PDF).GT.ERROR) GOTO 1
    IF(NPAN.LT.128) GOTO 1
2   RETURN
END
