      SUBROUTINE SROT (N, SX, INCX, SY, INCY, SC, SS)
      REAL SX, SY, SC, SS, ZERO, ONE, W, Z
      DIMENSION SX(*), SY(*)
      SAVE ZERO, ONE
      DATA ZERO, ONE /0.0E0, 1.0E0/
C***FIRST EXECUTABLE STATEMENT  SROT
      IF (N .LE. 0 .OR. (SS .EQ. ZERO .AND. SC .EQ. ONE)) GO TO 40
      IF (.NOT. (INCX .EQ. INCY .AND. INCX .GT. 0)) GO TO 20
C
C          Code for equal and positive increments.
C
           NSTEPS=INCX*N
           DO 10 I = 1,NSTEPS,INCX
                W=SX(I)
                Z=SY(I)
                SX(I)=SC*W+SS*Z
                SY(I)=-SS*W+SC*Z
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
                W=SX(KX)
                Z=SY(KY)
                SX(KX)=SC*W+SS*Z
                SY(KY)=-SS*W+SC*Z
                KX=KX+INCX
                KY=KY+INCY
   30           CONTINUE
   40 CONTINUE
C
      RETURN
      END
