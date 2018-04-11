      SUBROUTINE SINTI (N, WSAVE)
      DIMENSION WSAVE(*)
C***FIRST EXECUTABLE STATEMENT  SINTI
      IF (N .LE. 1) RETURN
      PI = 4.*ATAN(1.)
      NP1 = N+1
      NS2 = N/2
      DT = PI/NP1
      KS = N+2
      KF = KS+NS2-1
      FK = 0.
      DO 101 K=KS,KF
         FK = FK+1.
         WSAVE(K) = 2.*SIN(FK*DT)
  101 CONTINUE
      CALL RFFTI (NP1,WSAVE(KF+1))
      RETURN
      END
