      FUNCTION R9GMIC (A, X, ALX)
      SAVE EULER, EPS, BOT
      DATA EULER / .5772156649 015329 E0 /
      DATA EPS, BOT / 2*0.0 /
C***FIRST EXECUTABLE STATEMENT  R9GMIC
      IF (EPS.EQ.0.0) EPS = 0.5*R1MACH(3)
      IF (BOT.EQ.0.0) BOT = LOG(R1MACH(1))
C
      IF (A .GT. 0.0) CALL XERMSG ('SLATEC', 'R9GMIC',
     +   'A MUST BE NEAR A NEGATIVE INTEGER', 2, 2)
      IF (X .LE. 0.0) CALL XERMSG ('SLATEC', 'R9GMIC',
     +   'X MUST BE GT ZERO', 3, 2)
C
      MA = A - 0.5
      FM = -MA
      M = -MA
C
      TE = 1.0
      T = 1.0
      S = T
      DO 20 K=1,200
        FKP1 = K + 1
        TE = -X*TE/(FM+FKP1)
        T = TE/FKP1
        S = S + T
        IF (ABS(T).LT.EPS*S) GO TO 30
 20   CONTINUE
      CALL XERMSG ('SLATEC', 'R9GMIC',
     +   'NO CONVERGENCE IN 200 TERMS OF CONTINUED FRACTION', 4, 2)
C
 30   R9GMIC = -ALX - EULER + X*S/(FM+1.0)
      IF (M.EQ.0) RETURN
C
      IF (M.EQ.1) R9GMIC = -R9GMIC - 1.0 + 1.0/X
      IF (M.EQ.1) RETURN
C
      TE = FM
      T = 1.0
      S = T
      MM1 = M - 1
      DO 40 K=1,MM1
        FK = K
        TE = -X*TE/FK
        T = TE/(FM-FK)
        S = S + T
        IF (ABS(T).LT.EPS*ABS(S)) GO TO 50
 40   CONTINUE
C
 50   DO 60 K=1,M
        R9GMIC = R9GMIC + 1.0/K
 60   CONTINUE
C
      SGNG = 1.0
      IF (MOD(M,2).EQ.1) SGNG = -1.0
      ALNG = LOG(R9GMIC) - ALNGAM(FM+1.0)
C
      R9GMIC = 0.0
      IF (ALNG.GT.BOT) R9GMIC = SGNG*EXP(ALNG)
      IF (S.NE.0.0) R9GMIC = R9GMIC + SIGN (EXP(-FM*ALX+LOG(ABS(S)/FM))
     1  , S)
C
      IF (R9GMIC .EQ. 0.0 .AND. S .EQ. 0.0) CALL XERMSG ('SLATEC',
     +   'R9GMIC', 'RESULT UNDERFLOWS', 1, 1)
      RETURN
C
      END
