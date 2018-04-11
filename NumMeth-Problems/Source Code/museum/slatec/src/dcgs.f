      SUBROUTINE DCGS (N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSOLVE,
     +   ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, R0, P, Q, U, V1,
     +   V2, RWORK, IWORK)
C     .. Scalar Arguments ..
      DOUBLE PRECISION ERR, TOL
      INTEGER IERR, ISYM, ITER, ITMAX, ITOL, IUNIT, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), B(N), P(N), Q(N), R(N), R0(N), RWORK(*),
     +                 U(N), V1(N), V2(N), X(N)
      INTEGER IA(NELT), IWORK(*), JA(NELT)
C     .. Subroutine Arguments ..
      EXTERNAL MATVEC, MSOLVE
C     .. Local Scalars ..
      DOUBLE PRECISION AK, AKM, BK, BNRM, FUZZ, RHON, RHONM1, SIGMA,
     +                 SOLNRM, TOLMIN
      INTEGER I, K
C     .. External Functions ..
      DOUBLE PRECISION D1MACH, DDOT
      INTEGER ISDCGS
      EXTERNAL D1MACH, DDOT, ISDCGS
C     .. External Subroutines ..
      EXTERNAL DAXPY
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C***FIRST EXECUTABLE STATEMENT  DCGS
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
         V1(I)  = R(I) - B(I)
 10   CONTINUE
      CALL MSOLVE(N, V1, R, NELT, IA, JA, A, ISYM, RWORK, IWORK)
C
      IF( ISDCGS(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSOLVE,
     $     ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, R0, P, Q,
     $     U, V1, V2, RWORK, IWORK, AK, BK, BNRM, SOLNRM) .NE. 0 )
     $     GO TO 200
      IF( IERR.NE.0 ) RETURN
C
C         Set initial values.
C
      FUZZ = D1MACH(3)**2
      DO 20 I = 1, N
         R0(I) = R(I)
 20   CONTINUE
      RHONM1 = 1
C
C         ***** ITERATION LOOP *****
C
      DO 100 K=1,ITMAX
         ITER = K
C
C         Calculate coefficient BK and direction vectors U, V and P.
         RHON = DDOT(N, R0, 1, R, 1)
         IF( ABS(RHONM1).LT.FUZZ ) GOTO 998
         BK = RHON/RHONM1
         IF( ITER.EQ.1 ) THEN
            DO 30 I = 1, N
               U(I) = R(I)
               P(I) = R(I)
 30         CONTINUE
         ELSE
            DO 40 I = 1, N
               U(I) = R(I) + BK*Q(I)
               V1(I) = Q(I) + BK*P(I)
 40         CONTINUE
            DO 50 I = 1, N
               P(I) = U(I) + BK*V1(I)
 50         CONTINUE
         ENDIF
C
C         Calculate coefficient AK, new iterate X, Q
         CALL MATVEC(N, P, V2, NELT, IA, JA, A, ISYM)
         CALL MSOLVE(N, V2, V1, NELT, IA, JA, A, ISYM, RWORK, IWORK)
         SIGMA = DDOT(N, R0, 1, V1, 1)
         IF( ABS(SIGMA).LT.FUZZ ) GOTO 999
         AK = RHON/SIGMA
         AKM = -AK
         DO 60 I = 1, N
            Q(I) = U(I) + AKM*V1(I)
 60      CONTINUE
         DO 70 I = 1, N
            V1(I) = U(I) + Q(I)
 70      CONTINUE
C         X = X - ak*V1.
         CALL DAXPY( N, AKM, V1, 1, X, 1 )
C                     -1
C         R = R - ak*M  *A*V1
         CALL MATVEC(N, V1, V2, NELT, IA, JA, A, ISYM)
         CALL MSOLVE(N, V2, V1, NELT, IA, JA, A, ISYM, RWORK, IWORK)
         CALL DAXPY( N, AKM, V1, 1, R, 1 )
C
C         check stopping criterion.
         IF( ISDCGS(N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSOLVE,
     $        ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, R, R0, P, Q,
     $        U, V1, V2, RWORK, IWORK, AK, BK, BNRM, SOLNRM) .NE. 0 )
     $        GO TO 200
C
C         Update RHO.
         RHONM1 = RHON
 100  CONTINUE
C
C         *****   end of loop  *****
C         Stopping criterion not satisfied.
      ITER = ITMAX + 1
      IERR = 2
 200  RETURN
C
C         Breakdown of method detected.
 998  IERR = 5
      RETURN
C
C         Stagnation of method detected.
 999  IERR = 6
      RETURN
C------------- LAST LINE OF DCGS FOLLOWS ----------------------------
      END
