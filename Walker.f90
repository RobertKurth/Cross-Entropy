double precision function Walker(C_multiplier, m_multiplier, Source, C_retardation, C_in, DeltaK, R, m_in, n_in)
	use Program_Constants
	implicit none
    real*8, intent(IN)              ::  C_multiplier
    real*8, intent(IN)              ::  m_multiplier
    integer, intent(IN)             ::  Source
    real*8, intent(IN)              ::  C_in
    real*8, intent(IN)              ::  C_retardation
    real*8, intent(IN)              ::  DeltaK
    real*8, intent(IN)              ::  R
    real*8, intent(IN)              ::  m_in
    real*8, intent(IN)              ::  n_in
    real*8                          ::  C
    real*8                          ::  m
    real*8                          ::  n
    !
    !   Surce:  Data source
    !           0: User Input
    !           All other values are from 
    !              "Validation of Fatigue Models for ERW Seam Weld Cracking"
    !               Bruce A. Younf, Richard J. Olson, Jennifer M. O'Brien
    !               Proceedings of the ASME 2017 Pressure Vessels and Piping Conference
    !               PVP 2017        PVP2017-65378
    !               Waikoloa, Hawaii, USA
    !
    !           1: Weld Grade B
    !           2: Base Grade B
    !           3: Base Grade X52
    !           4: Base Grade X70
    !
    if(Source.eq.0) then
        C = C_in * C_retardation
        m = m_in
        n = One
    elseif(Source.eq.1) then
        C = 5.480D-11 * C_retardation
        m = 0.794D+00
        n = 3.454D+00
    elseif(Source.eq.2) then
        C = 6.000D-11 * C_retardation
        m = 0.879D+00
        n = 3.545D+00
    elseif(Source.eq.3) then
        C = 5.420D-11 * C_retardation
        m = 0.904D+00
        n = 3.500D+00
    elseif(Source.eq.4) then
        C = 4.800D-11 * C_retardation
        m = 0.932D+00
        n = 3.813D+00
    else
        write(*, *) 'Incrrect input for Walker model parameters'
       stop
    endif
    !
    !   Walker = da/dN
    !
    C = C * C_multiplier
    m = m * m_multiplier
    Walker = C * (DeltaK / (One - R)**(One - m))**n
    
end function Walker