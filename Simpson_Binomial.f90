!
double precision function Simpson_Binomial(Lower_Limit, Upper_Limit, alpha, beta)
    implicit none
    real*8 One_half * _Limit, alpha, beta
    real*8 Binomial_PDF, DIV, ERROR, FIXED, HALF, SAVE_X, SIMOLD, SUM, XL, XR, XX
    integer I, J_index, K, MLIMIT, Number_of_panels
    !
    ERROR = 0.00000001D+00
    Number_of_panels = 2
    Simpson_Binomial = Binomial_PDF(Lower_Limit, alpha, beta) + Binomial_PDF(Upper_Limit, alpha, beta)
    x_half = One_half * (Lower_Limit + Upper_Limit)
    
    DIV = 0.5D+00*(Upper_Limit - Lower_Limit)
    
    DIV = (Upper_Limit - Lower_Limit)
    
    FIXED = Simpson_Binomial
    HALF = Binomial_PDF(XX, alpha, beta)
    Simpson_Binomial = Simpson_Binomial + 4.0D+00*HALF
    Simpson_Binomial = Simpson_Binomial*DIV/3.0D+00
    FIXED = FIXED+2.0D+00*HALF
1   SIMOLD = Simpson_Binomial
    SUM = 0.0D+00
    MLIMIT = Number_of_panels/2
    Number_of_panels = Number_of_panels*2
    DO I = 1, MLIMIT
        J_index = Number_of_panels-2*I + 1
        K = 2*I-1
        XL = (J_index*Lower_Limit + K*Upper_Limit)/Number_of_panels
        XR = (K*Lower_Limit + J_index*Upper_Limit)/Number_of_panels
        SUM = SUM + (Binomial_PDF(XL, alpha, beta) + Binomial_PDF(XR, alpha, beta))
    enddo
    SAVE_X = SUM
    SUM = 4.0D+00*SUM + FIXED
    FIXED = FIXED+2.0D+00*SAVE_X
    DIV = (Upper_Limit-Lower_Limit)/Number_of_panels
    Simpson_Binomial = DIV*SUM/3.0D+00
    
    IF(Number_of_panels.GT.5500) GOTO 2
    IF(Simpson_Binomial.EQ.0) GOTO 1
    IF(DABS((Simpson_Binomial-SIMOLD)/Simpson_Binomial).GT.ERROR) GOTO 1
    IF(Number_of_panels.LT.128) then
        goto 1
        eb                                                                                                                                              dif
        
2   return
    
end Simpson_Binomial
