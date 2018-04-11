      INTEGER FUNCTION ISAMAX (N, SX, INCX)
      REAL SX(*), SMAX, XMAG
      INTEGER I, INCX, IX, N
C***FIRST EXECUTABLE STATEMENT  ISAMAX
      ISAMAX = 0
      IF (N .LE. 0) RETURN
      ISAMAX = 1
      IF (N .EQ. 1) RETURN
C
      IF (INCX .EQ. 1) GOTO 20
C
C     Code for increment not equal to 1.
C
      IX = 1
      IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
      SMAX = ABS(SX(IX))
      IX = IX + INCX
      DO 10 I = 2,N
        XMAG = ABS(SX(IX))
        IF (XMAG .GT. SMAX) THEN
          ISAMAX = I
          SMAX = XMAG
        ENDIF
        IX = IX + INCX
   10 CONTINUE
      RETURN
C
C     Code for increments equal to 1.
C
   20 SMAX = ABS(SX(1))
      DO 30 I = 2,N
        XMAG = ABS(SX(I))
        IF (XMAG .GT. SMAX) THEN
          ISAMAX = I
          SMAX = XMAG
        ENDIF
   30 CONTINUE
      RETURN
      END
