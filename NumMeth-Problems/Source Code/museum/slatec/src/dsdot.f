      DOUBLE PRECISION FUNCTION DSDOT (N, SX, INCX, SY, INCY)
      REAL SX(*),SY(*)
C***FIRST EXECUTABLE STATEMENT  DSDOT
      DSDOT = 0.0D0
      IF (N .LE. 0) RETURN
      IF (INCX.EQ.INCY .AND. INCX.GT.0) GO TO 20
C
C     Code for unequal or nonpositive increments.
C
      KX = 1
      KY = 1
      IF (INCX .LT. 0) KX = 1+(1-N)*INCX
      IF (INCY .LT. 0) KY = 1+(1-N)*INCY
      DO 10 I = 1,N
        DSDOT = DSDOT + DBLE(SX(KX))*DBLE(SY(KY))
        KX = KX + INCX
        KY = KY + INCY
   10 CONTINUE
      RETURN
C
C     Code for equal, positive, non-unit increments.
C
   20 NS = N*INCX
      DO 30 I = 1,NS,INCX
        DSDOT = DSDOT + DBLE(SX(I))*DBLE(SY(I))
   30 CONTINUE
      RETURN
      END
