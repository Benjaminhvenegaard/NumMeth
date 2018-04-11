      SUBROUTINE COSQI (N, WSAVE)
      DIMENSION WSAVE(*)
C***FIRST EXECUTABLE STATEMENT  COSQI
      PIH = 2.*ATAN(1.)
      DT = PIH/N
      FK = 0.
      DO 101 K=1,N
         FK = FK+1.
         WSAVE(K) = COS(FK*DT)
  101 CONTINUE
      CALL RFFTI (N,WSAVE(N+1))
      RETURN
      END
