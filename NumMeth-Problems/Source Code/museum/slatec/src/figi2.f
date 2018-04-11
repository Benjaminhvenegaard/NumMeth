      SUBROUTINE FIGI2 (NM, N, T, D, E, Z, IERR)
C
      INTEGER I,J,N,NM,IERR
      REAL T(NM,3),D(*),E(*),Z(NM,*)
      REAL H
C
C***FIRST EXECUTABLE STATEMENT  FIGI2
      IERR = 0
C
      DO 100 I = 1, N
C
         DO 50 J = 1, N
   50    Z(I,J) = 0.0E0
C
         IF (I .EQ. 1) GO TO 70
         H = T(I,1) * T(I-1,3)
         IF (H) 900, 60, 80
   60    IF (T(I,1) .NE. 0.0E0 .OR. T(I-1,3) .NE. 0.0E0) GO TO 1000
         E(I) = 0.0E0
   70    Z(I,I) = 1.0E0
         GO TO 90
   80    E(I) = SQRT(H)
         Z(I,I) = Z(I-1,I-1) * E(I) / T(I-1,3)
   90    D(I) = T(I,2)
  100 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- PRODUCT OF SOME PAIR OF OFF-DIAGONAL
C                ELEMENTS IS NEGATIVE ..........
  900 IERR = N + I
      GO TO 1001
C     .......... SET ERROR -- PRODUCT OF SOME PAIR OF OFF-DIAGONAL
C                ELEMENTS IS ZERO WITH ONE MEMBER NON-ZERO ..........
 1000 IERR = 2 * N + I
 1001 RETURN
      END
