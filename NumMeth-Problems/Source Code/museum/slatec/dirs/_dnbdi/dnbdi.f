      SUBROUTINE DNBDI (ABE, LDA, N, ML, MU, IPVT, DET)
      INTEGER LDA,N,ML,MU,IPVT(*)
      DOUBLE PRECISION ABE(LDA,*),DET(2)
C
      DOUBLE PRECISION TEN
      INTEGER I
C***FIRST EXECUTABLE STATEMENT  DNBDI
      DET(1) = 1.0D0
      DET(2) = 0.0D0
      TEN = 10.0D0
      DO 50 I = 1, N
         IF (IPVT(I) .NE. I) DET(1) = -DET(1)
         DET(1) = ABE(I,ML+1)*DET(1)
         IF (DET(1) .EQ. 0.0D0) GO TO 60
   10    IF (ABS(DET(1)) .GE. 1.0D0) GO TO 20
            DET(1) = TEN*DET(1)
            DET(2) = DET(2) - 1.0D0
         GO TO 10
   20    CONTINUE
   30    IF (ABS(DET(1)) .LT. TEN) GO TO 40
            DET(1) = DET(1)/TEN
            DET(2) = DET(2) + 1.0D0
         GO TO 30
   40    CONTINUE
   50 CONTINUE
   60 CONTINUE
      RETURN
      END
