      SUBROUTINE DSDCG (N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
     +   ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW)
C     .. Parameters ..
      INTEGER LOCRB, LOCIB
      PARAMETER (LOCRB=1, LOCIB=11)
C     .. Scalar Arguments ..
      DOUBLE PRECISION ERR, TOL
      INTEGER IERR, ISYM, ITER, ITMAX, ITOL, IUNIT, LENIW, LENW, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), B(N), RWORK(LENW), X(N)
      INTEGER IA(NELT), IWORK(LENIW), JA(NELT)
C     .. Local Scalars ..
      INTEGER LOCD, LOCDZ, LOCIW, LOCP, LOCR, LOCW, LOCZ
C     .. External Subroutines ..
      EXTERNAL DCG, DCHKW, DS2Y, DSDI, DSDS, DSMV
C***FIRST EXECUTABLE STATEMENT  DSDCG
C
      IERR = 0
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      ENDIF
C
C         Modify the SLAP matrix data structure to YSMP-Column.
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
C
C         Set up the work arrays.
      LOCIW = LOCIB
C
      LOCD = LOCRB
      LOCR = LOCD + N
      LOCZ = LOCR + N
      LOCP = LOCZ + N
      LOCDZ = LOCP + N
      LOCW  = LOCDZ + N
C
C         Check the workspace allocations.
      CALL DCHKW( 'DSDCG', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
C
      IWORK(4) = LOCD
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
C
C         Compute the inverse of the diagonal of the matrix.  This
C         will be used as the preconditioner.
      CALL DSDS(N, NELT, IA, JA, A, ISYM, RWORK(LOCD))
C
C         Do the Preconditioned Conjugate Gradient.
      CALL DCG(N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSDI,
     $     ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK(LOCR),
     $     RWORK(LOCZ), RWORK(LOCP), RWORK(LOCDZ), RWORK, IWORK)
      RETURN
C------------- LAST LINE OF DSDCG FOLLOWS -----------------------------
      END
