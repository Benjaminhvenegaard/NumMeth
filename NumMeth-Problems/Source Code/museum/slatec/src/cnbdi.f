      SUBROUTINE CNBDI (ABE, LDA, N, ML, MU, IPVT, DET)
      INTEGER LDA,N,ML,MU,IPVT(*)
      COMPLEX ABE(LDA,*),DET(2)
C
      REAL TEN
      INTEGER I
      COMPLEX ZDUM
      REAL CABS1
      CABS1(ZDUM) = ABS(REAL(ZDUM)) + ABS(AIMAG(ZDUM))
C
C***FIRST EXECUTABLE STATEMENT  CNBDI
      DET(1) = (1.0E0,0.0E0)
      DET(2) = (0.0E0,0.0E0)
      TEN = 10.0E0
      DO 50 I = 1, N
         IF (IPVT(I) .NE. I) DET(1) = -DET(1)
         DET(1) = ABE(I,ML+1)*DET(1)
         IF (CABS1(DET(1)) .EQ. 0.0E0) GO TO 60
   10    IF (CABS1(DET(1)) .GE. 1.0E0) GO TO 20
            DET(1) = CMPLX(TEN,0.0E0)*DET(1)
            DET(2) = DET(2) - (1.0E0,0.0E0)
         GO TO 10
   20    CONTINUE
   30    IF (CABS1(DET(1)) .LT. TEN) GO TO 40
            DET(1) = DET(1)/CMPLX(TEN,0.0E0)
            DET(2) = DET(2) + (1.0E0,0.0E0)
         GO TO 30
   40    CONTINUE
   50 CONTINUE
   60 CONTINUE
      RETURN
      END
