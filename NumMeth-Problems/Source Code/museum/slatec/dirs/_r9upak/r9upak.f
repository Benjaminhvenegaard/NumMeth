      SUBROUTINE R9UPAK (X, Y, N)
C***FIRST EXECUTABLE STATEMENT  R9UPAK
      ABSX = ABS(X)
      N = 0
      IF (X.EQ.0.0E0) GO TO 30
C
   10 IF (ABSX.GE.0.5E0) GO TO 20
      N = N-1
      ABSX = ABSX*2.0E0
      GO TO 10
C
   20 IF (ABSX.LT.1.0E0) GO TO 30
      N = N+1
      ABSX = ABSX*0.5E0
      GO TO 20
C
   30 Y = SIGN(ABSX,X)
      RETURN
C
      END
