      SUBROUTINE BAKVEC (NM, N, T, E, M, Z, IERR)
C
      INTEGER I,J,M,N,NM,IERR
      REAL T(NM,3),E(*),Z(NM,*)
C
C***FIRST EXECUTABLE STATEMENT  BAKVEC
      IERR = 0
      IF (M .EQ. 0) GO TO 1001
      E(1) = 1.0E0
      IF (N .EQ. 1) GO TO 1001
C
      DO 100 I = 2, N
         IF (E(I) .NE. 0.0E0) GO TO 80
         IF (T(I,1) .NE. 0.0E0 .OR. T(I-1,3) .NE. 0.0E0) GO TO 1000
         E(I) = 1.0E0
         GO TO 100
   80    E(I) = E(I-1) * E(I) / T(I-1,3)
  100 CONTINUE
C
      DO 120 J = 1, M
C
         DO 120 I = 2, N
         Z(I,J) = Z(I,J) * E(I)
  120 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- EIGENVECTORS CANNOT BE
C                FOUND BY THIS PROGRAM ..........
 1000 IERR = 2 * N + I
 1001 RETURN
      END
