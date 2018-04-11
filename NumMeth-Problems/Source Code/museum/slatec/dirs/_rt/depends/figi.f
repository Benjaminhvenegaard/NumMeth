      SUBROUTINE FIGI (NM, N, T, D, E, E2, IERR)
C
      INTEGER I,N,NM,IERR
      REAL T(NM,3),D(*),E(*),E2(*)
C
C***FIRST EXECUTABLE STATEMENT  FIGI
      IERR = 0
C
      DO 100 I = 1, N
         IF (I .EQ. 1) GO TO 90
         E2(I) = T(I,1) * T(I-1,3)
         IF (E2(I)) 1000, 60, 80
   60    IF (T(I,1) .EQ. 0.0E0 .AND. T(I-1,3) .EQ. 0.0E0) GO TO 80
C     .......... SET ERROR -- PRODUCT OF SOME PAIR OF OFF-DIAGONAL
C                ELEMENTS IS ZERO WITH ONE MEMBER NON-ZERO ..........
         IERR = -(3 * N + I)
   80    E(I) = SQRT(E2(I))
   90    D(I) = T(I,2)
  100 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- PRODUCT OF SOME PAIR OF OFF-DIAGONAL
C                ELEMENTS IS NEGATIVE ..........
 1000 IERR = N + I
 1001 RETURN
      END
