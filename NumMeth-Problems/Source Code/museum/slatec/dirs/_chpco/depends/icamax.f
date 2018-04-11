      INTEGER FUNCTION ICAMAX (N, CX, INCX)
      COMPLEX CX(*)
      REAL SMAX, XMAG
      INTEGER I, INCX, IX, N
      COMPLEX ZDUM
      REAL CABS1
      CABS1(ZDUM) = ABS(REAL(ZDUM)) + ABS(AIMAG(ZDUM))
C***FIRST EXECUTABLE STATEMENT  ICAMAX
      ICAMAX = 0
      IF (N .LE. 0) RETURN
      ICAMAX = 1
      IF (N .EQ. 1) RETURN
C
      IF (INCX .EQ. 1) GOTO 20
C
C     Code for increment not equal to 1.
C
      IX = 1
      IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
      SMAX = CABS1(CX(IX))
      IX = IX + INCX
      DO 10 I = 2,N
        XMAG = CABS1(CX(IX))
        IF (XMAG .GT. SMAX) THEN
          ICAMAX = I
          SMAX = XMAG
        ENDIF
        IX = IX + INCX
   10 CONTINUE
      RETURN
C
C     Code for increment equal to 1.
C
   20 SMAX = CABS1(CX(1))
      DO 30 I = 2,N
        XMAG = CABS1(CX(I))
        IF (XMAG .GT. SMAX) THEN
          ICAMAX = I
          SMAX = XMAG
        ENDIF
   30 CONTINUE
      RETURN
      END
