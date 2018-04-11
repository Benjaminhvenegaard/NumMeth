      SUBROUTINE COSTI (N, WSAVE)
      DIMENSION WSAVE(*)
C***FIRST EXECUTABLE STATEMENT  COSTI
      IF (N .LE. 3) RETURN
      PI = 4.*ATAN(1.)
      NM1 = N-1
      NP1 = N+1
      NS2 = N/2
      DT = PI/NM1
      FK = 0.
      DO 101 K=2,NS2
         KC = NP1-K
         FK = FK+1.
         WSAVE(K) = 2.*SIN(FK*DT)
         WSAVE(KC) = 2.*COS(FK*DT)
  101 CONTINUE
      CALL RFFTI (NM1,WSAVE(N+1))
      RETURN
      END
