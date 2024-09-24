!
DOUBLE PRECISION FUNCTION CDFTriag(z, a, c, b)
!
implicit none
real*8 z, a, b, c
!
  if(Z.lt.a) then
    CDFTriag = 0.0
    return
    endif
  if(Z.gt.b) then
    CDFTriag = 1.0
    return
    endif
  if(Z.gt.c) then
    CDFTriag = 1.0-(b-z)*(b-z)/((b-a)*(b-c))
  else
    CDFTriag = (z-a)*(z-a)/((b-a)*(c-a))
    endif
  RETURN
  END