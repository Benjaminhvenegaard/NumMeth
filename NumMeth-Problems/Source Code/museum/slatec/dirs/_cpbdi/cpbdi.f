      SUBROUTINE CPBDI (ABD, LDA, N, M, DET)
      INTEGER LDA,N,M
      COMPLEX ABD(LDA,*)
      REAL DET(2)
C
      REAL S
      INTEGER I
C***FIRST EXECUTABLE STATEMENT  CPBDI
C
C     COMPUTE DETERMINANT
C
      DET(1) = 1.0E0
      DET(2) = 0.0E0
      S = 10.0E0
      DO 50 I = 1, N
         DET(1) = REAL(ABD(M+1,I))**2*DET(1)
         IF (DET(1) .EQ. 0.0E0) GO TO 60
   10    IF (DET(1) .GE. 1.0E0) GO TO 20
            DET(1) = S*DET(1)
            DET(2) = DET(2) - 1.0E0
         GO TO 10
   20    CONTINUE
   30    IF (DET(1) .LT. S) GO TO 40
            DET(1) = DET(1)/S
            DET(2) = DET(2) + 1.0E0
         GO TO 30
   40    CONTINUE
   50 CONTINUE
   60 CONTINUE
      RETURN
      END
