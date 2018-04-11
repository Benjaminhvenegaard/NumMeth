      DOUBLE PRECISION FUNCTION D9LGIC (A, X, ALX)
      DOUBLE PRECISION A, X, ALX, EPS, FK, P, R, S, T, XMA, XPA, D1MACH
      SAVE EPS
      DATA EPS / 0.D0 /
C***FIRST EXECUTABLE STATEMENT  D9LGIC
      IF (EPS.EQ.0.D0) EPS = 0.5D0*D1MACH(3)
C
      XPA = X + 1.0D0 - A
      XMA = X - 1.D0 - A
C
      R = 0.D0
      P = 1.D0
      S = P
      DO 10 K=1,300
        FK = K
        T = FK*(A-FK)*(1.D0+R)
        R = -T/((XMA+2.D0*FK)*(XPA+2.D0*FK)+T)
        P = R*P
        S = S + P
        IF (ABS(P).LT.EPS*S) GO TO 20
 10   CONTINUE
      CALL XERMSG ('SLATEC', 'D9LGIC',
     +   'NO CONVERGENCE IN 300 TERMS OF CONTINUED FRACTION', 1, 2)
C
 20   D9LGIC = A*ALX - X + LOG(S/XPA)
C
      RETURN
      END
