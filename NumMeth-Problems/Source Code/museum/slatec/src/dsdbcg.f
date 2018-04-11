      SUBROUTINE DSDBCG (N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
     +   ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW)
C     .. Parameters ..
      INTEGER LOCRB, LOCIB
      PARAMETER (LOCRB=1, LOCIB=11)
C     .. Scalar Arguments ..
      DOUBLE PRECISION ERR, TOL
      INTEGER IERR, ISYM, ITER, ITMAX, ITOL, IUNIT, LENIW, LENW, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(N), B(N), RWORK(LENW), X(N)
      INTEGER IA(NELT), IWORK(LENIW), JA(NELT)
C     .. Local Scalars ..
      INTEGER LOCDIN, LOCDZ, LOCIW, LOCP, LOCPP, LOCR, LOCRR, LOCW,
     +        LOCZ, LOCZZ
C     .. External Subroutines ..
      EXTERNAL DBCG, DCHKW, DS2Y, DSDI, DSDS, DSMTV, DSMV
C***FIRST EXECUTABLE STATEMENT  DSDBCG
C
      IERR = 0
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      ENDIF
C
C         Change the SLAP input matrix IA, JA, A to SLAP-Column format.
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
C
C         Set up the workspace.
      LOCIW = LOCIB
C
      LOCDIN = LOCRB
      LOCR = LOCDIN + N
      LOCZ = LOCR + N
      LOCP = LOCZ + N
      LOCRR = LOCP + N
      LOCZZ = LOCRR + N
      LOCPP = LOCZZ + N
      LOCDZ = LOCPP + N
      LOCW = LOCDZ + N
C
C         Check the workspace allocations.
      CALL DCHKW( 'DSDBCG', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
C
      IWORK(4) = LOCDIN
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
C
C         Compute the inverse of the diagonal of the matrix.
      CALL DSDS(N, NELT, IA, JA, A, ISYM, RWORK(LOCDIN))
C
C         Perform the Diagonally Scaled BiConjugate gradient algorithm.
      CALL DBCG(N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSMTV,
     $     DSDI, DSDI, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT,
     $     RWORK(LOCR), RWORK(LOCZ), RWORK(LOCP),
     $     RWORK(LOCRR), RWORK(LOCZZ), RWORK(LOCPP),
     $     RWORK(LOCDZ), RWORK(1), IWORK(1))
      RETURN
C------------- LAST LINE OF DSDBCG FOLLOWS ----------------------------
      END
