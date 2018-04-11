      SUBROUTINE STIN (N, NELT, IA, JA, A, ISYM, SOLN, RHS, IUNIT, JOB)
C     .. Scalar Arguments ..
      INTEGER ISYM, IUNIT, JOB, N, NELT
C     .. Array Arguments ..
      REAL A(NELT), RHS(N), SOLN(N)
      INTEGER IA(NELT), JA(NELT)
C     .. Local Scalars ..
      INTEGER I, IRHS, ISOLN, JOBRET, NELTMX
C     .. Intrinsic Functions ..
      INTRINSIC MIN
C***FIRST EXECUTABLE STATEMENT  STIN
C
C         Read in the information heading.
C
      NELTMX = NELT
      READ(IUNIT,1000) N, NELT, ISYM, IRHS, ISOLN
      NELT = MIN( NELT, NELTMX )
C
C         Read in the matrix non-zeros in Triad format.
      DO 10 I = 1, NELT
         READ(IUNIT,1010) IA(I), JA(I), A(I)
 10   CONTINUE
C
C         If requested, read in the rhs.
      JOBRET = 0
      IF( JOB.EQ.1 .OR. JOB.EQ.3 ) THEN
C
C         Check to see if rhs is in the file.
         IF( IRHS.EQ.1 ) THEN
            JOBRET = 1
            READ(IUNIT,1020) (RHS(I),I=1,N)
         ELSE
            DO 20 I = 1, N
               RHS(I) = 0
 20         CONTINUE
         ENDIF
      ENDIF
C
C         If requested, read in the solution.
      IF( JOB.GT.1 ) THEN
C
C         Check to see if solution is in the file.
         IF( ISOLN.EQ.1 ) THEN
            JOBRET = JOBRET + 2
            READ(IUNIT,1020) (SOLN(I),I=1,N)
         ELSE
            DO 30 I = 1, N
               SOLN(I) = 0
 30         CONTINUE
         ENDIF
      ENDIF
C
      JOB = JOBRET
      RETURN
 1000 FORMAT(5I10)
 1010 FORMAT(1X,I5,1X,I5,1X,E16.7)
 1020 FORMAT(1X,E16.7)
C------------- LAST LINE OF STIN FOLLOWS ----------------------------
      END
