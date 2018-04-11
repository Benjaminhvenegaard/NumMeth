      SUBROUTINE CDSCL (HMAX, N, NQ, RMAX, H, RC, RH, YH)
      INTEGER I, J, N, NQ
      COMPLEX YH(N,*)
      REAL H, HMAX, RC, RH, RMAX, R1
C***FIRST EXECUTABLE STATEMENT  CDSCL
      IF (H .LT. 1.E0) THEN
        RH = MIN(ABS(H)*RH, ABS(H)*RMAX, HMAX)/ABS(H)
      ELSE
        RH = MIN(RH, RMAX, HMAX/ABS(H))
      END IF
      R1 = 1.E0
      DO 10 J = 1,NQ
        R1 = R1*RH
        DO 10 I = 1,N
 10       YH(I,J+1) = YH(I,J+1)*R1
      H = H*RH
      RC = RC*RH
      RETURN
      END
