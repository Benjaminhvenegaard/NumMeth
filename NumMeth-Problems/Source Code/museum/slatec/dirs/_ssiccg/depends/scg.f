      SUBROUTINE SCG (N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSOLVE,
     +   ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, DZ, RWORK,
     +   IWORK)
C     .. Scalar Arguments ..
      REAL ERR, TOL
      INTEGER IERR, ISYM, ITER, ITMAX, ITOL, IUNIT, N, NELT
C     .. Array Arguments ..
      REAL A(NELT), B(N), DZ(N), P(N), R(N), RWORK(*), X(N), Z(N)
      INTEGER IA(NELT), IWORK(*), JA(NELT)
C     .. Subroutine Arguments ..
      EXTERNAL MATVEC, MSOLVE
C     .. Local Scalars ..
      REAL AK, AKDEN, BK, BKDEN, BKNUM, BNRM, SOLNRM, TOLMIN
      INTEGER I, K
C     .. External Functions ..
      REAL R1MACH, SDOT
      INTEGER ISSCG
      EXTERNAL R1MACH, SDOT, ISSCG
C     .. External Subroutines ..
      EXTERNAL SAXPY, SCOPY
C***FIRST EXECUTABLE STATEMENT  SCG
C
C         Check some of the input data.
C
      ITER = 0
      IERR = 0
      IF( N.LT.1 ) THEN
         IERR = 3
         RETURN
      ENDIF
      TOLMIN = 500*R1MACH(3)
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
      IF( ISSCG(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL,
     $     ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, DZ,
     $     RWORK, IWORK, AK, BK, BNRM, SOLNRM) .NE. 0 ) GO TO 200
      IF( IERR.NE.0 ) RETURN
C
C         ***** Iteration loop *****
C
      DO 100 K=1,ITMAX
         ITER = K
C
C         Calculate coefficient bk and direction vector p.
         BKNUM = SDOT(N, Z, 1, R, 1)
         IF( BKNUM.LE.0.0E0 ) THEN
            IERR = 5
            RETURN
         ENDIF
         IF(ITER .EQ. 1) THEN
            CALL SCOPY(N, Z, 1, P, 1)
         ELSE
            BK = BKNUM/BKDEN
            DO 20 I = 1, N
               P(I) = Z(I) + BK*P(I)
 20         CONTINUE
         ENDIF
         BKDEN = BKNUM
C
C         Calculate coefficient ak, new iterate x, new residual r,
C         and new pseudo-residual z.
         CALL MATVEC(N, P, Z, NELT, IA, JA, A, ISYM)
         AKDEN = SDOT(N, P, 1, Z, 1)
         IF( AKDEN.LE.0.0E0 ) THEN
            IERR = 6
            RETURN
         ENDIF
         AK = BKNUM/AKDEN
         CALL SAXPY(N, AK, P, 1, X, 1)
         CALL SAXPY(N, -AK, Z, 1, R, 1)
         CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
C
C         check stopping criterion.
         IF( ISSCG(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL,
     $        ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, DZ, RWORK,
     $        IWORK, AK, BK, BNRM, SOLNRM) .NE. 0 ) GO TO 200
C
 100  CONTINUE
C
C         *****   end of loop  *****
C
C         stopping criterion not satisfied.
      ITER = ITMAX + 1
      IERR = 2
C
 200  RETURN
C------------- LAST LINE OF SCG FOLLOWS -----------------------------
      END
