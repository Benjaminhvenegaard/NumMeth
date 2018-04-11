      SUBROUTINE SSDBCG (N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
     +   ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW)
C     .. Parameters ..
      INTEGER LOCRB, LOCIB
      PARAMETER (LOCRB=1, LOCIB=11)
C     .. Scalar Arguments ..
      REAL ERR, TOL
      INTEGER IERR, ISYM, ITER, ITMAX, ITOL, IUNIT, LENIW, LENW, N, NELT
C     .. Array Arguments ..
      REAL A(N), B(N), RWORK(LENW), X(N)
      INTEGER IA(NELT), IWORK(LENIW), JA(NELT)
C     .. Local Scalars ..
      INTEGER LOCDIN, LOCDZ, LOCIW, LOCP, LOCPP, LOCR, LOCRR, LOCW,
     +        LOCZ, LOCZZ
C     .. External Subroutines ..
      EXTERNAL SBCG, SCHKW, SS2Y, SSDI, SSDS, SSMTV, SSMV
C***FIRST EXECUTABLE STATEMENT  SSDBCG
C
      IERR = 0
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      ENDIF
C
C         Change the SLAP input matrix IA, JA, A to SLAP-Column format.
      CALL SS2Y( N, NELT, IA, JA, A, ISYM )
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
      CALL SCHKW( 'SSDBCG', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
C
      IWORK(4) = LOCDIN
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
C
C         Compute the inverse of the diagonal of the matrix.
      CALL SSDS(N, NELT, IA, JA, A, ISYM, RWORK(LOCDIN))
C
C         Perform the Diagonally Scaled BiConjugate gradient algorithm.
      CALL SBCG(N, B, X, NELT, IA, JA, A, ISYM, SSMV, SSMTV,
     $     SSDI, SSDI, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT,
     $     RWORK(LOCR), RWORK(LOCZ), RWORK(LOCP),
     $     RWORK(LOCRR), RWORK(LOCZZ), RWORK(LOCPP),
     $     RWORK(LOCDZ), RWORK(1), IWORK(1))
      RETURN
C------------- LAST LINE OF SSDBCG FOLLOWS ----------------------------
      END
