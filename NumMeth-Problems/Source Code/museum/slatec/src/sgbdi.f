      SUBROUTINE SGBDI (ABD, LDA, N, ML, MU, IPVT, DET)
      INTEGER LDA,N,ML,MU,IPVT(*)
      REAL ABD(LDA,*),DET(2)
C
      REAL TEN
      INTEGER I,M
C***FIRST EXECUTABLE STATEMENT  SGBDI
      M = ML + MU + 1
      DET(1) = 1.0E0
      DET(2) = 0.0E0
      TEN = 10.0E0
      DO 50 I = 1, N
         IF (IPVT(I) .NE. I) DET(1) = -DET(1)
         DET(1) = ABD(M,I)*DET(1)
         IF (DET(1) .EQ. 0.0E0) GO TO 60
   10    IF (ABS(DET(1)) .GE. 1.0E0) GO TO 20
            DET(1) = TEN*DET(1)
            DET(2) = DET(2) - 1.0E0
         GO TO 10
   20    CONTINUE
   30    IF (ABS(DET(1)) .LT. TEN) GO TO 40
            DET(1) = DET(1)/TEN
            DET(2) = DET(2) + 1.0E0
         GO TO 30
   40    CONTINUE
   50 CONTINUE
   60 CONTINUE
      RETURN
      END
