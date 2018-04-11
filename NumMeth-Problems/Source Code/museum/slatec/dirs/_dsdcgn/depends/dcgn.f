      SUBROUTINE DCGN (N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MTTVEC,
     +   MSOLVE, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, ATP,
     +   ATZ, DZ, ATDZ, RWORK, IWORK)
C     .. Scalar Arguments ..
      DOUBLE PRECISION ERR, TOL
      INTEGER IERR, ISYM, ITER, ITMAX, ITOL, IUNIT, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(N), ATDZ(N), ATP(N), ATZ(N), B(N), DZ(N), P(N),
     +                 R(N), RWORK(*), X(N), Z(N)
      INTEGER IA(NELT), IWORK(*), JA(NELT)
C     .. Subroutine Arguments ..
      EXTERNAL MATVEC, MSOLVE, MTTVEC
C     .. Local Scalars ..
      DOUBLE PRECISION AK, AKDEN, BK, BKDEN, BKNUM, BNRM, SOLNRM, TOLMIN
      INTEGER I, K
C     .. External Functions ..
      DOUBLE PRECISION D1MACH, DDOT
      INTEGER ISDCGN
      EXTERNAL D1MACH, DDOT, ISDCGN
C     .. External Subroutines ..
      EXTERNAL DAXPY, DCOPY
C***FIRST EXECUTABLE STATEMENT  DCGN
C
C         Check user input.
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
C         Calculate initial residual and pseudo-residual, and check
C         stopping criterion.
      CALL MATVEC(N, X, R, NELT, IA, JA, A, ISYM)
      DO 10 I = 1, N
         R(I) = B(I) - R(I)
 10   CONTINUE
      CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
      CALL MTTVEC(N, Z, ATZ, NELT, IA, JA, A, ISYM)
C
      IF( ISDCGN(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MTTVEC, MSOLVE,
     $     ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, ATP, ATZ,
     $     DZ, ATDZ, RWORK, IWORK, AK, BK, BNRM, SOLNRM) .NE. 0 )
     $     GO TO 200
      IF( IERR.NE.0 ) RETURN
C
C         ***** iteration loop *****
C
      DO 100 K=1,ITMAX
         ITER = K
C
C         Calculate coefficient BK and direction vector P.
         BKNUM = DDOT(N, Z, 1, R, 1)
         IF( BKNUM.LE.0.0D0 ) THEN
            IERR = 6
            RETURN
         ENDIF
         IF(ITER .EQ. 1) THEN
            CALL DCOPY(N, Z, 1, P, 1)
         ELSE
            BK = BKNUM/BKDEN
            DO 20 I = 1, N
               P(I) = Z(I) + BK*P(I)
 20         CONTINUE
         ENDIF
         BKDEN = BKNUM
C
C         Calculate coefficient AK, new iterate X, new residual R,
C         and new pseudo-residual ATZ.
         IF(ITER .NE. 1) CALL DAXPY(N, BK, ATP, 1, ATZ, 1)
         CALL DCOPY(N, ATZ, 1, ATP, 1)
         AKDEN = DDOT(N, ATP, 1, ATP, 1)
         IF( AKDEN.LE.0.0D0 ) THEN
            IERR = 6
            RETURN
         ENDIF
         AK = BKNUM/AKDEN
         CALL DAXPY(N, AK, ATP, 1, X, 1)
         CALL MATVEC(N, ATP, Z, NELT, IA, JA, A, ISYM)
         CALL DAXPY(N, -AK, Z, 1, R, 1)
         CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
         CALL MTTVEC(N, Z, ATZ, NELT, IA, JA, A, ISYM)
C
C         check stopping criterion.
         IF( ISDCGN(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MTTVEC,
     $        MSOLVE, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R,
     $        Z, P, ATP, ATZ, DZ, ATDZ, RWORK, IWORK, AK, BK, BNRM,
     $        SOLNRM) .NE. 0) GOTO 200
C
 100  CONTINUE
C
C         *****   end of loop  *****
C
C         stopping criterion not satisfied.
      ITER = ITMAX + 1
C
 200  RETURN
C------------- LAST LINE OF DCGN FOLLOWS ----------------------------
      END
