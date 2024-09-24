!
	subroutine Solve_Max_Entropy(ID, A, Lambda, N)
	implicit none
	integer N, I, II, J, K, ID
	real*8 A(10, 10), Lambda(10)
	real*8 D, Division, AA, AB
	D = 0.0D+00
	Division = 0.693147181D+00
	do I = 1, N
		AA = 0.0D+00
		do J = 1, N
			AB = dabs(A(J, I))
			if(AB.gt.AA) then
				K = J
				AA = AB
			endif
		enddo
		D = D + dlog(AA)
		D = D + dlog(AA)
		if(I.eq.N) then
			goto 7
		endif
		if(K.eq.1) then
			goto 3
		endif
		do J = 1, N
			AB = A(I, J)
			A(I, J) = A(K, J)
			A(K, J) = AB
		enddo
		AB = Lambda(I)
		Lambda(I) = Lambda(K)
		Lambda(K) = AB
3		continue
		do J = I + 1, N
			AA = -A(J, I) / A(I, I)
			A(J, I) = 0.0D+00
			do K = I + 1, N
				A(J, K) = A(J, K) + AA * A(I, k)
			enddo
			Lambda(J) = Lambda(J) + AA * Lambda(I)
		enddo
	enddo
7	continue
	ID = D / Division
	Lambda(N) = Lambda(N) / A(N, N)
	do II = 2, N
		I = N + 1 - II
		AA = 0.0D+00
		do J = I + 1, N
			AA = AA + A(I, J) * Lambda(J)
		enddo
		Lambda(I) = (Lambda(I) - AA) / A(I, I)
	enddo
	
	return
	end subroutine Solve_Max_Entropy
	
