      SUBROUTINE SINT (N, X, WSAVE)
      DIMENSION X(*), WSAVE(*)
C***FIRST EXECUTABLE STATEMENT  SINT
      SQRT3 = SQRT(3.)
      IF (N-2) 101,102,103
  101 X(1) = X(1)+X(1)
      RETURN
  102 XH = SQRT3*(X(1)+X(2))
      X(2) = SQRT3*(X(1)-X(2))
      X(1) = XH
      RETURN
  103 NP1 = N+1
      NS2 = N/2
      WSAVE(1) = 0.
      KW = NP1
      DO 104 K=1,NS2
         KW = KW+1
         KC = NP1-K
         T1 = X(K)-X(KC)
         T2 = WSAVE(KW)*(X(K)+X(KC))
         WSAVE(K+1) = T1+T2
         WSAVE(KC+1) = T2-T1
  104 CONTINUE
      MODN = MOD(N,2)
      IF (MODN .NE. 0) WSAVE(NS2+2) = 4.*X(NS2+1)
      NF = NP1+NS2+1
      CALL RFFTF (NP1,WSAVE,WSAVE(NF))
      X(1) = .5*WSAVE(1)
      DO 105 I=3,N,2
         X(I-1) = -WSAVE(I)
         X(I) = X(I-2)+WSAVE(I-1)
  105 CONTINUE
      IF (MODN .NE. 0) RETURN
      X(N) = -WSAVE(N+1)
      RETURN
      END
