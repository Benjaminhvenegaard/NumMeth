      DOUBLE PRECISION FUNCTION ZABS (ZR, ZI)
      DOUBLE PRECISION ZR, ZI, U, V, Q, S
C***FIRST EXECUTABLE STATEMENT  ZABS
      U = ABS(ZR)
      V = ABS(ZI)
      S = U + V
C-----------------------------------------------------------------------
C     S*1.0D0 MAKES AN UNNORMALIZED UNDERFLOW ON CDC MACHINES INTO A
C     TRUE FLOATING ZERO
C-----------------------------------------------------------------------
      S = S*1.0D+0
      IF (S.EQ.0.0D+0) GO TO 20
      IF (U.GT.V) GO TO 10
      Q = U/V
      ZABS = V*SQRT(1.D+0+Q*Q)
      RETURN
   10 Q = V/U
      ZABS = U*SQRT(1.D+0+Q*Q)
      RETURN
   20 ZABS = 0.0D+0
      RETURN
      END
