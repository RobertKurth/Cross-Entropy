subroutine Sort_Index_Integer(Array, Index_Sorted, N)
	implicit none
  integer, intent(OUT), dimension(N) :: Index_Sorted
	integer, dimension(N) :: Array

	integer IH, CH
  integer N, Lab, L, M, I, J
	
	DO I = 1, N
		Index_Sorted(I) = I
	enddo
	
	!***********************************************************************
  !****	****
  !**** THIS PROGRAM WILL SORT INTEGER OR FLOATING POINT NUMBERS.	****
  !**** DECLARE CH AND real*8 OR INTEGER AS NEEDED.	****
  !****	****
  !***********************************************************************
  !
  IF(N.LT.2) goto 9
  ASSIGN 1 TO LAB
  L = N/2 + 1
  M = N
1 IF(L.NE.1) GOTO 2
  ASSIGN 3 TO LAB
  GOTO 3
2 L = L-1
  CH = Array(L)
  IH = Index_Sorted(L)
  GOTO 4
3 CH = Array(M)
  IH = Index_Sorted(M)
	Array(M) = Array(1)
  Index_Sorted(M) = Index_Sorted(1)
  M = M-1
  IF(M.NE.1) GOTO 4
  Array(1) = CH
  Index_Sorted(1) = IH
	goto 9
4 J = L
5 I = J
  J = J + J
  IF(J-M)6, 7, 8
6 IF(Array(J).GE.Array(J + 1)) GOTO 7
  J = J + 1
7 IF(CH.GE.Array(J)) GOTO 8
  Array(I) = Array(J)
  Index_Sorted(I) = Index_Sorted(J)
  GOTO 5
8 Array(I) = CH
  Index_Sorted(I) = IH
  GOTO LAB, (1, 3)
9 continue	
end subroutine Sort_Index_Integer
