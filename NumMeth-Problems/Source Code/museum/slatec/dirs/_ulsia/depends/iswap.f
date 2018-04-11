      SUBROUTINE ISWAP (N, IX, INCX, IY, INCY)
      INTEGER IX(*), IY(*), ITEMP1, ITEMP2, ITEMP3
C***FIRST EXECUTABLE STATEMENT  ISWAP
      IF (N .LE. 0) RETURN
      IF (INCX .NE. INCY) GO TO 5
      IF (INCX-1) 5,20,60
C
C     Code for unequal or nonpositive increments.
C
    5 IIX = 1
      IIY = 1
      IF (INCX .LT. 0) IIX = (1-N)*INCX + 1
      IF (INCY .LT. 0) IIY = (1-N)*INCY + 1
      DO 10 I = 1,N
        ITEMP1 = IX(IIX)
        IX(IIX) = IY(IIY)
        IY(IIY) = ITEMP1
        IIX = IIX + INCX
        IIY = IIY + INCY
   10 CONTINUE
      RETURN
C
C     Code for both increments equal to 1.
C
C     Clean-up loop so remaining vector length is a multiple of 3.
C
   20 M = MOD(N,3)
      IF (M .EQ. 0) GO TO 40
      DO 30 I = 1,M
        ITEMP1 = IX(I)
        IX(I) = IY(I)
        IY(I) = ITEMP1
   30 CONTINUE
      IF (N .LT. 3) RETURN
   40 MP1 = M + 1
      DO 50 I = MP1,N,3
        ITEMP1 = IX(I)
        ITEMP2 = IX(I+1)
        ITEMP3 = IX(I+2)
        IX(I) = IY(I)
        IX(I+1) = IY(I+1)
        IX(I+2) = IY(I+2)
        IY(I) = ITEMP1
        IY(I+1) = ITEMP2
        IY(I+2) = ITEMP3
   50 CONTINUE
      RETURN
C
C     Code for equal, positive, non-unit increments.
C
   60 NS = N*INCX
      DO 70 I = 1,NS,INCX
        ITEMP1 = IX(I)
        IX(I) = IY(I)
        IY(I) = ITEMP1
   70 CONTINUE
      RETURN
      END
