      FUNCTION R9LGIT (A, X, ALGAP1)
      SAVE EPS, SQEPS
      DATA EPS, SQEPS / 2*0.0 /
C***FIRST EXECUTABLE STATEMENT  R9LGIT
      IF (EPS.EQ.0.0) EPS = 0.5*R1MACH(3)
      IF (SQEPS.EQ.0.0) SQEPS = SQRT(R1MACH(4))
C
      IF (X .LE. 0.0 .OR. A .LT. X) CALL XERMSG ('SLATEC', 'R9LGIT',
     +   'X SHOULD BE GT 0.0 AND LE A', 2, 2)
C
      AX = A + X
      A1X = AX + 1.0
      R = 0.0
      P = 1.0
      S = P
      DO 20 K=1,200
        FK = K
        T = (A+FK)*X*(1.0+R)
        R = T/((AX+FK)*(A1X+FK)-T)
        P = R*P
        S = S + P
        IF (ABS(P).LT.EPS*S) GO TO 30
 20   CONTINUE
      CALL XERMSG ('SLATEC', 'R9LGIT',
     +   'NO CONVERGENCE IN 200 TERMS OF CONTINUED FRACTION', 3, 2)
C
 30   HSTAR = 1.0 - X*S/A1X
      IF (HSTAR .LT. SQEPS) CALL XERMSG ('SLATEC', 'R9LGIT',
     +   'RESULT LESS THAN HALF PRECISION', 1, 1)
C
      R9LGIT = -X - ALGAP1 - LOG(HSTAR)
C
      RETURN
      END
