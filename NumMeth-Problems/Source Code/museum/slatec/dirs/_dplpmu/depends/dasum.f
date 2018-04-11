      DOUBLE PRECISION FUNCTION DASUM (N, DX, INCX)
      DOUBLE PRECISION DX(*)
      INTEGER I, INCX, IX, M, MP1, N
C***FIRST EXECUTABLE STATEMENT  DASUM
      DASUM = 0.0D0
      IF (N .LE. 0) RETURN
C
      IF (INCX .EQ. 1) GOTO 20
C
C     Code for increment not equal to 1.
C
      IX = 1
      IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
      DO 10 I = 1,N
        DASUM = DASUM + ABS(DX(IX))
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
        DASUM = DASUM + ABS(DX(I))
   30 CONTINUE
      IF (N .LT. 6) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,6
        DASUM = DASUM + ABS(DX(I)) + ABS(DX(I+1)) + ABS(DX(I+2)) +
     1          ABS(DX(I+3)) + ABS(DX(I+4)) + ABS(DX(I+5))
   50 CONTINUE
      RETURN
      END
