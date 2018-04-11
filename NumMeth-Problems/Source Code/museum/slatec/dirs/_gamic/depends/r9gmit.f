      FUNCTION R9GMIT (A, X, ALGAP1, SGNGAM, ALX)
      SAVE EPS, BOT
      DATA EPS, BOT / 2*0.0 /
C***FIRST EXECUTABLE STATEMENT  R9GMIT
      IF (EPS.EQ.0.0) EPS = 0.5*R1MACH(3)
      IF (BOT.EQ.0.0) BOT = LOG(R1MACH(1))
C
      IF (X .LE. 0.0) CALL XERMSG ('SLATEC', 'R9GMIT',
     +   'X SHOULD BE GT 0', 1, 2)
C
      MA = A + 0.5
      IF (A.LT.0.0) MA = A - 0.5
      AEPS = A - MA
C
      AE = A
      IF (A.LT.(-0.5)) AE = AEPS
C
      T = 1.0
      TE = AE
      S = T
      DO 20 K=1,200
        FK = K
        TE = -X*TE/FK
        T = TE/(AE+FK)
        S = S + T
        IF (ABS(T).LT.EPS*ABS(S)) GO TO 30
 20   CONTINUE
      CALL XERMSG ('SLATEC', 'R9GMIT',
     +   'NO CONVERGENCE IN 200 TERMS OF TAYLOR-S SERIES', 2, 2)
C
 30   IF (A.GE.(-0.5)) ALGS = -ALGAP1 + LOG(S)
      IF (A.GE.(-0.5)) GO TO 60
C
      ALGS = -ALNGAM(1.0+AEPS) + LOG(S)
      S = 1.0
      M = -MA - 1
      IF (M.EQ.0) GO TO 50
      T = 1.0
      DO 40 K=1,M
        T = X*T/(AEPS-M-1+K)
        S = S + T
        IF (ABS(T).LT.EPS*ABS(S)) GO TO 50
 40   CONTINUE
C
 50   R9GMIT = 0.0
      ALGS = -MA*LOG(X) + ALGS
      IF (S.EQ.0.0 .OR. AEPS.EQ.0.0) GO TO 60
C
      SGNG2 = SGNGAM*SIGN(1.0,S)
      ALG2 = -X - ALGAP1 + LOG(ABS(S))
C
      IF (ALG2.GT.BOT) R9GMIT = SGNG2*EXP(ALG2)
      IF (ALGS.GT.BOT) R9GMIT = R9GMIT + EXP(ALGS)
      RETURN
C
 60   R9GMIT = EXP(ALGS)
      RETURN
C
      END
