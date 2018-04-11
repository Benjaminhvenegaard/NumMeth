      SUBROUTINE CUCHK (Y, NZ, ASCLE, TOL)
C
      COMPLEX Y
      REAL ASCLE, SS, ST, TOL, YR, YI
      INTEGER NZ
C***FIRST EXECUTABLE STATEMENT  CUCHK
      NZ = 0
      YR = REAL(Y)
      YI = AIMAG(Y)
      YR = ABS(YR)
      YI = ABS(YI)
      ST = MIN(YR,YI)
      IF (ST.GT.ASCLE) RETURN
      SS = MAX(YR,YI)
      ST=ST/TOL
      IF (SS.LT.ST) NZ = 1
      RETURN
      END
