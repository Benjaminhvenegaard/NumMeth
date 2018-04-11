      REAL FUNCTION SASUM (N, SX, INCX)
      REAL SX(*)
      INTEGER I, INCX, IX, M, MP1, N
C***FIRST EXECUTABLE STATEMENT  SASUM
      SASUM = 0.0E0
      IF (N .LE. 0) RETURN
C
      IF (INCX .EQ. 1) GOTO 20
C
C     Code for increment not equal to 1.
C
      IX = 1
      IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
      DO 10 I = 1,N
        SASUM = SASUM + ABS(SX(IX))
        IX = IX + INCX
   10 CONTINUE
      RETURN
C
C     Code for increment equal to 1.
C
C     Clean-up loop so remaining vector length is a multiple of 6.
C
   20 M = MOD(N,6)
      IF (M .EQ. 0) GOTO 40
      DO 30 I = 1,M
        SASUM = SASUM + ABS(SX(I))
   30 CONTINUE
      IF (N .LT. 6) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,6
        SASUM = SASUM + ABS(SX(I)) + ABS(SX(I+1)) + ABS(SX(I+2)) +
     1          ABS(SX(I+3)) + ABS(SX(I+4)) + ABS(SX(I+5))
   50 CONTINUE
      RETURN
      END
