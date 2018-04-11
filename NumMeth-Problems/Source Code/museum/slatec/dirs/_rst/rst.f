      SUBROUTINE RST (NM, N, W, E, MATZ, Z, IERR)
C
      INTEGER I,J,N,NM,IERR,MATZ
      REAL W(*),E(*),Z(NM,*)
C
C***FIRST EXECUTABLE STATEMENT  RST
      IF (N .LE. NM) GO TO 10
      IERR = 10 * N
      GO TO 50
C
   10 IF (MATZ .NE. 0) GO TO 20
C     .......... FIND EIGENVALUES ONLY ..........
      CALL  IMTQL1(N,W,E,IERR)
      GO TO 50
C     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS ..........
   20 DO 40 I = 1, N
C
         DO 30 J = 1, N
            Z(J,I) = 0.0E0
   30    CONTINUE
C
         Z(I,I) = 1.0E0
   40 CONTINUE
C
      CALL  IMTQL2(NM,N,W,E,Z,IERR)
   50 RETURN
      END
