      REAL FUNCTION SDSDOT (N, SB, SX, INCX, SY, INCY)
      REAL SX(*), SY(*), SB
      DOUBLE PRECISION DSDOT
C***FIRST EXECUTABLE STATEMENT  SDSDOT
      DSDOT = SB
      IF (N .LE. 0) GO TO 30
      IF (INCX.EQ.INCY .AND. INCX.GT.0) GO TO 40
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
   30 SDSDOT = DSDOT
      RETURN
C
C     Code for equal and positive increments.
C
   40 NS = N*INCX
      DO 50 I = 1,NS,INCX
        DSDOT = DSDOT + DBLE(SX(I))*DBLE(SY(I))
   50 CONTINUE
      SDSDOT = DSDOT
      RETURN
      END
