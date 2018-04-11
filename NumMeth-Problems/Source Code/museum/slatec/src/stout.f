      SUBROUTINE STOUT (N, NELT, IA, JA, A, ISYM, SOLN, RHS, IUNIT, JOB)
C     .. Scalar Arguments ..
      INTEGER ISYM, IUNIT, JOB, N, NELT
C     .. Array Arguments ..
      REAL A(NELT), RHS(N), SOLN(N)
      INTEGER IA(NELT), JA(NELT)
C     .. Local Scalars ..
      INTEGER I, IRHS, ISOLN
C***FIRST EXECUTABLE STATEMENT  STOUT
C
C         If RHS and SOLN are to be printed also.
C         Write out the information heading.
C
      IRHS = 0
      ISOLN = 0
      IF( JOB.EQ.1 .OR. JOB.EQ.3 ) IRHS = 1
      IF( JOB.GT.1 ) ISOLN = 1
      WRITE(IUNIT,1000) N, NELT, ISYM, IRHS, ISOLN
C
C         Write out the matrix non-zeros in Triad format.
      DO 10 I = 1, NELT
         WRITE(IUNIT,1010) IA(I), JA(I), A(I)
 10   CONTINUE
C
C         If requested, write out the rhs.
      IF( IRHS.EQ.1 ) THEN
         WRITE(IUNIT,1020) (RHS(I),I=1,N)
      ENDIF
C
C         If requested, write out the solution.
      IF( ISOLN.EQ.1 ) THEN
         WRITE(IUNIT,1020) (SOLN(I),I=1,N)
      ENDIF
      RETURN
 1000 FORMAT(5I10)
 1010 FORMAT(1X,I5,1X,I5,1X,E16.7)
 1020 FORMAT(1X,E16.7)
C------------- LAST LINE OF STOUT FOLLOWS ----------------------------
      END
