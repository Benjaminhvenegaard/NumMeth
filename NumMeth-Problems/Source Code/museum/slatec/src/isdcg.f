      INTEGER FUNCTION ISDCG (N, B, X, NELT, IA, JA, A, ISYM, MSOLVE,
     +   ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, DZ, RWORK,
     +   IWORK, AK, BK, BNRM, SOLNRM)
C     .. Scalar Arguments ..
      DOUBLE PRECISION AK, BK, BNRM, ERR, SOLNRM, TOL
      INTEGER IERR, ISYM, ITER, ITMAX, ITOL, IUNIT, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), B(N), DZ(N), P(N), R(N), RWORK(*), X(N),
     +                 Z(N)
      INTEGER IA(NELT), IWORK(*), JA(NELT)
C     .. Subroutine Arguments ..
      EXTERNAL MSOLVE
C     .. Arrays in Common ..
      DOUBLE PRECISION SOLN(1)
C     .. Local Scalars ..
      INTEGER I
C     .. External Functions ..
      DOUBLE PRECISION D1MACH, DNRM2
      EXTERNAL D1MACH, DNRM2
C     .. Common blocks ..
      COMMON /DSLBLK/ SOLN
C***FIRST EXECUTABLE STATEMENT  ISDCG
      ISDCG = 0
C
      IF( ITOL.EQ.1 ) THEN
C         err = ||Residual||/||RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) BNRM = DNRM2(N, B, 1)
         ERR = DNRM2(N, R, 1)/BNRM
      ELSE IF( ITOL.EQ.2 ) THEN
C                  -1              -1
C         err = ||M  Residual||/||M  RightHandSide|| (2-Norms).
         IF(ITER .EQ. 0) THEN
            CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
            BNRM = DNRM2(N, DZ, 1)
         ENDIF
         ERR = DNRM2(N, Z, 1)/BNRM
      ELSE IF( ITOL.EQ.11 ) THEN
C         err = ||x-TrueSolution||/||TrueSolution|| (2-Norms).
         IF(ITER .EQ. 0) SOLNRM = DNRM2(N, SOLN, 1)
         DO 10 I = 1, N
            DZ(I) = X(I) - SOLN(I)
 10      CONTINUE
         ERR = DNRM2(N, DZ, 1)/SOLNRM
      ELSE
C
C         If we get here ITOL is not one of the acceptable values.
         ERR = D1MACH(2)
         IERR = 3
      ENDIF
C
      IF(IUNIT .NE. 0) THEN
         IF( ITER.EQ.0 ) THEN
            WRITE(IUNIT,1000) N, ITOL
            WRITE(IUNIT,1010) ITER, ERR
         ELSE
            WRITE(IUNIT,1010) ITER, ERR, AK, BK
         ENDIF
      ENDIF
      IF(ERR .LE. TOL) ISDCG = 1
      RETURN
 1000 FORMAT(' Preconditioned Conjugate Gradient for ',
     $     'N, ITOL = ',I5, I5,
     $     /' ITER','   Error Estimate','            Alpha',
     $     '             Beta')
 1010 FORMAT(1X,I4,1X,D16.7,1X,D16.7,1X,D16.7)
C------------- LAST LINE OF ISDCG FOLLOWS ------------------------------
      END
