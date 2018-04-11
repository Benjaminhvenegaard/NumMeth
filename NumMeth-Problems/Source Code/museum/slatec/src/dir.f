      SUBROUTINE DIR (N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSOLVE,
     +   ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, DZ, RWORK,
     +   IWORK)
C     .. Scalar Arguments ..
      DOUBLE PRECISION ERR, TOL
      INTEGER IERR, ISYM, ITER, ITMAX, ITOL, IUNIT, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), B(N), DZ(N), R(N), RWORK(*), X(N), Z(N)
      INTEGER IA(NELT), IWORK(*), JA(NELT)
C     .. Subroutine Arguments ..
      EXTERNAL MATVEC, MSOLVE
C     .. Local Scalars ..
      DOUBLE PRECISION BNRM, SOLNRM, TOLMIN
      INTEGER I, K
C     .. External Functions ..
      DOUBLE PRECISION D1MACH
      INTEGER ISDIR
      EXTERNAL D1MACH, ISDIR
C***FIRST EXECUTABLE STATEMENT  DIR
C
C         Check some of the input data.
C
      ITER = 0
      IERR = 0
      IF( N.LT.1 ) THEN
         IERR = 3
         RETURN
      ENDIF
      TOLMIN = 500*D1MACH(3)
      IF( TOL.LT.TOLMIN ) THEN
         TOL = TOLMIN
         IERR = 4
      ENDIF
C
C         Calculate initial residual and pseudo-residual, and check
C         stopping criterion.
      CALL MATVEC(N, X, R, NELT, IA, JA, A, ISYM)
      DO 10 I = 1, N
         R(I) = B(I) - R(I)
 10   CONTINUE
      CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
C
      IF( ISDIR(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL,
     $     ITMAX, ITER, ERR, IERR, IUNIT, R, Z, DZ, RWORK,
     $     IWORK, BNRM, SOLNRM) .NE. 0 ) GO TO 200
      IF( IERR.NE.0 ) RETURN
C
C         ***** iteration loop *****
C
      DO 100 K=1,ITMAX
         ITER = K
C
C         Calculate new iterate x, new residual r, and new
C         pseudo-residual z.
         DO 20 I = 1, N
            X(I) = X(I) + Z(I)
 20      CONTINUE
         CALL MATVEC(N, X, R, NELT, IA, JA, A, ISYM)
         DO 30 I = 1, N
            R(I) = B(I) - R(I)
 30      CONTINUE
         CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
C
C         check stopping criterion.
         IF( ISDIR(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL,
     $        ITMAX, ITER, ERR, IERR, IUNIT, R, Z, DZ, RWORK,
     $        IWORK, BNRM, SOLNRM) .NE. 0 ) GO TO 200
C
 100  CONTINUE
C
C         *****   end of loop  *****
C         Stopping criterion not satisfied.
      ITER = ITMAX + 1
      IERR = 2
C
 200  RETURN
C------------- LAST LINE OF DIR FOLLOWS -------------------------------
      END
