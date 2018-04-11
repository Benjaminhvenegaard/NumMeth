      SUBROUTINE DCDOT (N, FM, CX, INCX, CY, INCY, DCR, DCI)
      INTEGER I, INCX, INCY, KX, KY, N
      COMPLEX CX(*), CY(*)
      DOUBLE PRECISION DCR, DCI, DT1, DT2, DT3, DT4, FM
C***FIRST EXECUTABLE STATEMENT  DCDOT
      DCR = 0.0D0
      DCI = 0.0D0
      IF (N .LE. 0) GO TO 20
C
      KX = 1
      KY = 1
      IF (INCX .LT. 0) KX = 1+(1-N)*INCX
      IF (INCY .LT. 0) KY = 1+(1-N)*INCY
      DO 10 I = 1,N
        DT1 = DBLE(REAL(CX(KX)))
        DT2 = DBLE(REAL(CY(KY)))
        DT3 = DBLE(AIMAG(CX(KX)))
        DT4 = DBLE(AIMAG(CY(KY)))
        DCR = DCR+(DT1*DT2)-FM*(DT3*DT4)
        DCI = DCI+(DT1*DT4)+FM*(DT3*DT2)
        KX = KX+INCX
        KY = KY+INCY
   10 CONTINUE
   20 RETURN
      END
