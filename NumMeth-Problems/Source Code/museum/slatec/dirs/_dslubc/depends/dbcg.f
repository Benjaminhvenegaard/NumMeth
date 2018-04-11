      SUBROUTINE DBCG (N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MTTVEC,
     +   MSOLVE, MTSOLV, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, Z,
     +   P, RR, ZZ, PP, DZ, RWORK, IWORK)
C     .. Scalar Arguments ..
      DOUBLE PRECISION ERR, TOL
      INTEGER IERR, ISYM, ITER, ITMAX, ITOL, IUNIT, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), B(N), DZ(N), P(N), PP(N), R(N), RR(N),
     +                 RWORK(*), X(N), Z(N), ZZ(N)
      INTEGER IA(NELT), IWORK(*), JA(NELT)
C     .. Subroutine Arguments ..
      EXTERNAL MATVEC, MSOLVE, MTSOLV, MTTVEC
C     .. Local Scalars ..
      DOUBLE PRECISION AK, AKDEN, BK, BKDEN, BKNUM, BNRM, FUZZ, SOLNRM,
     +                 TOLMIN
      INTEGER I, K
C     .. External Functions ..
      DOUBLE PRECISION D1MACH, DDOT
      INTEGER ISDBCG
      EXTERNAL D1MACH, DDOT, ISDBCG
C     .. External Subroutines ..
      EXTERNAL DAXPY, DCOPY
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C***FIRST EXECUTABLE STATEMENT  DBCG
C
C         Check some of the input data.
C
      ITER = 0
      IERR = 0
      IF( N.LT.1 ) THEN
         IERR = 3
         RETURN
      ENDIF
      FUZZ = D1MACH(3)
      TOLMIN = 500*FUZZ
      FUZZ = FUZZ*FUZZ
      IF( TOL.LT.TOLMIN ) THEN
         TOL = TOLMIN
         IERR = 4
      ENDIF
C
C         Calculate initial residual and pseudo-residual, and check
C         stopping criterion.
      CALL MATVEC(N, X, R, NELT, IA, JA, A, ISYM)
      DO 10 I = 1, N
         R(I)  = B(I) - R(I)
         RR(I) = R(I)
 10   CONTINUE
      CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
      CALL MTSOLV(N, RR, ZZ, NELT, IA, JA, A, ISYM, RWORK, IWORK)
C
      IF( ISDBCG(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL,
     $     ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, RR, ZZ, PP,
     $     DZ, RWORK, IWORK, AK, BK, BNRM, SOLNRM) .NE. 0 )
     $     GO TO 200
      IF( IERR.NE.0 ) RETURN
C
C         ***** iteration loop *****
C
      DO 100 K=1,ITMAX
         ITER = K
C
C         Calculate coefficient BK and direction vectors P and PP.
         BKNUM = DDOT(N, Z, 1, RR, 1)
         IF( ABS(BKNUM).LE.FUZZ ) THEN
            IERR = 6
            RETURN
         ENDIF
         IF(ITER .EQ. 1) THEN
            CALL DCOPY(N, Z, 1, P, 1)
            CALL DCOPY(N, ZZ, 1, PP, 1)
         ELSE
            BK = BKNUM/BKDEN
            DO 20 I = 1, N
               P(I) = Z(I) + BK*P(I)
               PP(I) = ZZ(I) + BK*PP(I)
 20         CONTINUE
         ENDIF
         BKDEN = BKNUM
C
C         Calculate coefficient AK, new iterate X, new residuals R and
C         RR, and new pseudo-residuals Z and ZZ.
         CALL MATVEC(N, P, Z, NELT, IA, JA, A, ISYM)
         AKDEN = DDOT(N, PP, 1, Z, 1)
         AK = BKNUM/AKDEN
         IF( ABS(AKDEN).LE.FUZZ ) THEN
            IERR = 6
            RETURN
         ENDIF
         CALL DAXPY(N, AK, P, 1, X, 1)
         CALL DAXPY(N, -AK, Z, 1, R, 1)
         CALL MTTVEC(N, PP, ZZ, NELT, IA, JA, A, ISYM)
         CALL DAXPY(N, -AK, ZZ, 1, RR, 1)
         CALL MSOLVE(N, R, Z, NELT, IA, JA, A, ISYM, RWORK, IWORK)
         CALL MTSOLV(N, RR, ZZ, NELT, IA, JA, A, ISYM, RWORK, IWORK)
C
C         check stopping criterion.
         IF( ISDBCG(N, B, X, NELT, IA, JA, A, ISYM, MSOLVE, ITOL, TOL,
     $        ITMAX, ITER, ERR, IERR, IUNIT, R, Z, P, RR, ZZ,
     $        PP, DZ, RWORK, IWORK, AK, BK, BNRM, SOLNRM) .NE. 0 )
     $        GO TO 200
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
C------------- LAST LINE OF DBCG FOLLOWS ----------------------------
      END
