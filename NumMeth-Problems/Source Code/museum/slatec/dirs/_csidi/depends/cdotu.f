      COMPLEX FUNCTION CDOTU (N, CX, INCX, CY, INCY)
      COMPLEX CX(*),CY(*)
C***FIRST EXECUTABLE STATEMENT  CDOTU
      CDOTU = (0.0,0.0)
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
        CDOTU = CDOTU + CX(KX)*CY(KY)
        KX = KX + INCX
        KY = KY + INCY
   10 CONTINUE
      RETURN
C
C     Code for equal, positive increments.
C
   20 NS = N*INCX
      DO 30 I = 1,NS,INCX
        CDOTU = CDOTU + CX(I)*CY(I)
   30 CONTINUE
      RETURN
      END
