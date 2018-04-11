      SUBROUTINE DROT (N, DX, INCX, DY, INCY, DC, DS)
      DOUBLE PRECISION DX, DY, DC, DS, ZERO, ONE, W, Z
      DIMENSION DX(*), DY(*)
      SAVE ZERO, ONE
      DATA ZERO, ONE /0.0D0, 1.0D0/
C***FIRST EXECUTABLE STATEMENT  DROT
      IF (N .LE. 0 .OR. (DS .EQ. ZERO .AND. DC .EQ. ONE)) GO TO 40
      IF (.NOT. (INCX .EQ. INCY .AND. INCX .GT. 0)) GO TO 20
C
C          Code for equal and positive increments.
C
           NSTEPS=INCX*N
           DO 10 I = 1,NSTEPS,INCX
                W=DX(I)
                Z=DY(I)
                DX(I)=DC*W+DS*Z
                DY(I)=-DS*W+DC*Z
   10           CONTINUE
           GO TO 40
C
C     Code for unequal or nonpositive increments.
C
   20 CONTINUE
           KX=1
           KY=1
C
           IF (INCX .LT. 0) KX = 1-(N-1)*INCX
           IF (INCY .LT. 0) KY = 1-(N-1)*INCY
C
           DO 30 I = 1,N
                W=DX(KX)
                Z=DY(KY)
                DX(KX)=DC*W+DS*Z
                DY(KY)=-DS*W+DC*Z
                KX=KX+INCX
                KY=KY+INCY
   30           CONTINUE
   40 CONTINUE
C
      RETURN
      END
