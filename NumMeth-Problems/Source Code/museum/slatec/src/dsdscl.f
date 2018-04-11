      SUBROUTINE DSDSCL (N, NELT, IA, JA, A, ISYM, X, B, DINV, JOB,
     +   ITOL)
C     .. Scalar Arguments ..
      INTEGER ISYM, ITOL, JOB, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), B(N), DINV(N), X(N)
      INTEGER IA(NELT), JA(NELT)
C     .. Arrays in Common ..
      DOUBLE PRECISION SOLN(1)
C     .. Local Scalars ..
      DOUBLE PRECISION DI
      INTEGER ICOL, J, JBGN, JEND
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C     .. Common blocks ..
      COMMON /DSLBLK/ SOLN
C***FIRST EXECUTABLE STATEMENT  DSDSCL
C
C         SCALING...
C
      IF( JOB.NE.0 ) THEN
         DO 10 ICOL = 1, N
            DINV(ICOL) = 1.0D0/SQRT( A(JA(ICOL)) )
 10      CONTINUE
      ELSE
C
C         UNSCALING...
C
         DO 15 ICOL = 1, N
            DINV(ICOL) = 1.0D0/DINV(ICOL)
 15      CONTINUE
      ENDIF
C
      DO 30 ICOL = 1, N
         JBGN = JA(ICOL)
         JEND = JA(ICOL+1)-1
         DI = DINV(ICOL)
         DO 20 J = JBGN, JEND
            A(J) = DINV(IA(J))*A(J)*DI
 20      CONTINUE
 30   CONTINUE
C
      DO 40 ICOL = 1, N
         B(ICOL) = B(ICOL)*DINV(ICOL)
         X(ICOL) = X(ICOL)/DINV(ICOL)
 40   CONTINUE
C
C         Check to see if we need to scale the "true solution" as well.
C
      IF( ITOL.EQ.11 ) THEN
         DO 50 ICOL = 1, N
            SOLN(ICOL) = SOLN(ICOL)/DINV(ICOL)
 50      CONTINUE
      ENDIF
C
      RETURN
C------------- LAST LINE OF DSDSCL FOLLOWS ----------------------------
      END
