      DOUBLE PRECISION FUNCTION DGAMIC (A, X)
      DOUBLE PRECISION A, X, AEPS, AINTA, ALGAP1, ALNEPS, ALNGS, ALX,
     1  BOT, E, EPS, GSTAR, H, SGA, SGNG, SGNGAM, SGNGS, SQEPS, T,
     2  D1MACH, DLNGAM, D9GMIC, D9GMIT, D9LGIC, D9LGIT
      LOGICAL FIRST
      SAVE EPS, SQEPS, ALNEPS, BOT, FIRST
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  DGAMIC
      IF (FIRST) THEN
         EPS = 0.5D0*D1MACH(3)
         SQEPS = SQRT(D1MACH(4))
         ALNEPS = -LOG (D1MACH(3))
         BOT = LOG (D1MACH(1))
      ENDIF
      FIRST = .FALSE.
C
      IF (X .LT. 0.D0) CALL XERMSG ('SLATEC', 'DGAMIC', 'X IS NEGATIVE'
     +   , 2, 2)
C
      IF (X.GT.0.D0) GO TO 20
      IF (A .LE. 0.D0) CALL XERMSG ('SLATEC', 'DGAMIC',
     +   'X = 0 AND A LE 0 SO DGAMIC IS UNDEFINED', 3, 2)
C
      DGAMIC = EXP (DLNGAM(A+1.D0) - LOG(A))
      RETURN
C
 20   ALX = LOG (X)
      SGA = 1.0D0
      IF (A.NE.0.D0) SGA = SIGN (1.0D0, A)
      AINTA = AINT (A + 0.5D0*SGA)
      AEPS = A - AINTA
C
      IZERO = 0
      IF (X.GE.1.0D0) GO TO 40
C
      IF (A.GT.0.5D0 .OR. ABS(AEPS).GT.0.001D0) GO TO 30
      E = 2.0D0
      IF (-AINTA.GT.1.D0) E = 2.D0*(-AINTA+2.D0)/(AINTA*AINTA-1.0D0)
      E = E - ALX * X**(-0.001D0)
      IF (E*ABS(AEPS).GT.EPS) GO TO 30
C
      DGAMIC = D9GMIC (A, X, ALX)
      RETURN
C
 30   CALL DLGAMS (A+1.0D0, ALGAP1, SGNGAM)
      GSTAR = D9GMIT (A, X, ALGAP1, SGNGAM, ALX)
      IF (GSTAR.EQ.0.D0) IZERO = 1
      IF (GSTAR.NE.0.D0) ALNGS = LOG (ABS(GSTAR))
      IF (GSTAR.NE.0.D0) SGNGS = SIGN (1.0D0, GSTAR)
      GO TO 50
C
 40   IF (A.LT.X) DGAMIC = EXP (D9LGIC(A, X, ALX))
      IF (A.LT.X) RETURN
C
      SGNGAM = 1.0D0
      ALGAP1 = DLNGAM (A+1.0D0)
      SGNGS = 1.0D0
      ALNGS = D9LGIT (A, X, ALGAP1)
C
C EVALUATION OF DGAMIC(A,X) IN TERMS OF TRICOMI-S INCOMPLETE GAMMA FN.
C
 50   H = 1.D0
      IF (IZERO.EQ.1) GO TO 60
C
      T = A*ALX + ALNGS
      IF (T.GT.ALNEPS) GO TO 70
      IF (T.GT.(-ALNEPS)) H = 1.0D0 - SGNGS*EXP(T)
C
      IF (ABS(H).LT.SQEPS) CALL XERCLR
      IF (ABS(H) .LT. SQEPS) CALL XERMSG ('SLATEC', 'DGAMIC',
     +   'RESULT LT HALF PRECISION', 1, 1)
C
 60   SGNG = SIGN (1.0D0, H) * SGA * SGNGAM
      T = LOG(ABS(H)) + ALGAP1 - LOG(ABS(A))
      IF (T.LT.BOT) CALL XERCLR
      DGAMIC = SGNG * EXP(T)
      RETURN
C
 70   SGNG = -SGNGS * SGA * SGNGAM
      T = T + ALGAP1 - LOG(ABS(A))
      IF (T.LT.BOT) CALL XERCLR
      DGAMIC = SGNG * EXP(T)
      RETURN
C
      END
