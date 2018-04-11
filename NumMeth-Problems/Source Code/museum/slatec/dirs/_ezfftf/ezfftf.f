      SUBROUTINE EZFFTF (N, R, AZERO, A, B, WSAVE)
      DIMENSION R(*), A(*), B(*), WSAVE(*)
C***FIRST EXECUTABLE STATEMENT  EZFFTF
      IF (N-2) 101,102,103
  101 AZERO = R(1)
      RETURN
  102 AZERO = .5*(R(1)+R(2))
      A(1) = .5*(R(1)-R(2))
      RETURN
  103 DO 104 I=1,N
         WSAVE(I) = R(I)
  104 CONTINUE
      CALL RFFTF (N,WSAVE,WSAVE(N+1))
      CF = 2./N
      CFM = -CF
      AZERO = .5*CF*WSAVE(1)
      NS2 = (N+1)/2
      NS2M = NS2-1
      DO 105 I=1,NS2M
         A(I) = CF*WSAVE(2*I)
         B(I) = CFM*WSAVE(2*I+1)
  105 CONTINUE
      IF (MOD(N,2) .EQ. 0) A(NS2) = .5*CF*WSAVE(N)
      RETURN
      END
