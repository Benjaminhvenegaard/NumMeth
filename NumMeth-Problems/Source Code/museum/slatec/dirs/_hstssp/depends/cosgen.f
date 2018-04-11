      SUBROUTINE COSGEN (N, IJUMP, FNUM, FDEN, A)
      DIMENSION       A(*)
C
C
C***FIRST EXECUTABLE STATEMENT  COSGEN
      PI = PIMACH(DUM)
      IF (N .EQ. 0) GO TO 105
      IF (IJUMP .EQ. 1) GO TO 103
      K3 = N/IJUMP+1
      K4 = K3-1
      PIBYN = PI/(N+IJUMP)
      DO 102 K=1,IJUMP
         K1 = (K-1)*K3
         K5 = (K-1)*K4
         DO 101 I=1,K4
            X = K1+I
            K2 = K5+I
            A(K2) = -2.*COS(X*PIBYN)
  101    CONTINUE
  102 CONTINUE
      GO TO 105
  103 CONTINUE
      NP1 = N+1
      Y = PI/(N+FDEN)
      DO 104 I=1,N
         X = NP1-I-FNUM
         A(I) = 2.*COS(X*Y)
  104 CONTINUE
  105 CONTINUE
      RETURN
      END
