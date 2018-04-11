      INTEGER FUNCTION ISSCGS (N, B, X, NELT, IA, JA, A, ISYM, MATVEC,
     +   MSOLVE, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, R0, P, Q,
     +   U, V1, V2, RWORK, IWORK, AK, BK, BNRM, SOLNRM)
C     .. Scalar Arguments ..
      REAL AK, BK, BNRM, ERR, SOLNRM, TOL
      INTEGER IERR, ISYM, ITER, ITMAX, ITOL, IUNIT, N, NELT
C     .. Array Arguments ..
      REAL A(NELT), B(N), P(N), Q(N), R(N), R0(N), RWORK(*), U(N),
     +     V1(N), V2(N), X(N)
      INTEGER IA(NELT), IWORK(*), JA(NELT)
C     .. Subroutine Arguments ..
      EXTERNAL MATVEC, MSOLVE
C     .. Arrays in Common ..
      REAL SOLN(1)
C     .. Local Scalars ..
      INTEGER I
C     .. External Functions ..
      REAL R1MACH, SNRM2
      EXTERNAL R1MACH, SNRM2
C     .. Common blocks ..
      COMMON /SSLBLK/ SOLN
C***FIRST EXECUTABLE STATEMENT  ISSCGS
      ISSCGS = 0
C
      IF( ITOL.EQ.1 ) THEN
C         err = ||Residual||/||RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) BNRM = SNRM2(N, B, 1)
         CALL MATVEC(N, X, V2, NELT, IA, JA, A, ISYM )
         DO 5 I = 1, N
            V2(I) = V2(I) - B(I)
 5       CONTINUE
         ERR = SNRM2(N, V2, 1)/BNRM
      ELSE IF( ITOL.EQ.2 ) THEN
C                  -1              -1
C         err = ||M  Residual||/||M  RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) THEN
            CALL MSOLVE(N, B, V2, NELT, IA, JA, A, ISYM, RWORK, IWORK)
            BNRM = SNRM2(N, V2, 1)
         ENDIF
         ERR = SNRM2(N, R, 1)/BNRM
      ELSE IF( ITOL.EQ.11 ) THEN
C         err = ||x-TrueSolution||/||TrueSolution|| (2-Norms).
         IF(ITER .EQ. 0) SOLNRM = SNRM2(N, SOLN, 1)
         DO 10 I = 1, N
            V2(I) = X(I) - SOLN(I)
 10      CONTINUE
         ERR = SNRM2(N, V2, 1)/SOLNRM
      ELSE
C
C         If we get here ITOL is not one of the acceptable values.
         ERR = R1MACH(2)
         IERR = 3
      ENDIF
C
C         Print the error and coefficients AK, BK on each step,
C         if desired.
      IF(IUNIT .NE. 0) THEN
         IF( ITER.EQ.0 ) THEN
            WRITE(IUNIT,1000) N, ITOL
            WRITE(IUNIT,1010) ITER, ERR
         ELSE
            WRITE(IUNIT,1010) ITER, ERR, AK, BK
         ENDIF
      ENDIF
      IF(ERR .LE. TOL) ISSCGS = 1
C
      RETURN
 1000 FORMAT(' Preconditioned BiConjugate Gradient Squared for ',
     $     'N, ITOL = ',I5, I5,
     $     /' ITER','   Error Estimate','            Alpha',
     $     '             Beta')
 1010 FORMAT(1X,I4,1X,E16.7,1X,E16.7,1X,E16.7)
C------------- LAST LINE OF ISSCGS FOLLOWS ----------------------------
      END
