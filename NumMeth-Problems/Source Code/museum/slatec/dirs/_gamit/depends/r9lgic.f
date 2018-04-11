      FUNCTION R9LGIC (A, X, ALX)
      SAVE EPS
      DATA EPS / 0.0 /
C***FIRST EXECUTABLE STATEMENT  R9LGIC
      IF (EPS.EQ.0.0) EPS = 0.5*R1MACH(3)
C
      XPA = X + 1.0 - A
      XMA = X - 1.0 - A
C
      R = 0.0
      P = 1.0
      S = P
      DO 10 K=1,200
        FK = K
        T = FK*(A-FK)*(1.0+R)
        R = -T/((XMA+2.0*FK)*(XPA+2.0*FK)+T)
        P = R*P
        S = S + P
        IF (ABS(P).LT.EPS*S) GO TO 20
 10   CONTINUE
      CALL XERMSG ('SLATEC', 'R9LGIC',
     +   'NO CONVERGENCE IN 200 TERMS OF CONTINUED FRACTION', 1, 2)
C
 20   R9LGIC = A*ALX - X + LOG(S/XPA)
C
      RETURN
      END
