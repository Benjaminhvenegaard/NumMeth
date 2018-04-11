      FUNCTION PRVEC (M, U, V)
C
      DIMENSION U(*),V(*)
C***FIRST EXECUTABLE STATEMENT  PRVEC
      N=M/2
      NP=N+1
      VP=SDOT(N,U(1),1,V(NP),1)
      PRVEC=SDOT(N,U(NP),1,V(1),1) - VP
      RETURN
      END
