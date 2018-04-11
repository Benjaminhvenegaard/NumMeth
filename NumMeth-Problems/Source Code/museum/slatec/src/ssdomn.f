      SUBROUTINE SSDOMN (N, B, X, NELT, IA, JA, A, ISYM, NSAVE, ITOL,
     +   TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW)
C     .. Parameters ..
      INTEGER LOCRB, LOCIB
      PARAMETER (LOCRB=1, LOCIB=11)
C     .. Scalar Arguments ..
      REAL ERR, TOL
      INTEGER IERR, ISYM, ITER, ITMAX, ITOL, IUNIT, LENIW, LENW, N,
     +        NELT, NSAVE
C     .. Array Arguments ..
      REAL A(N), B(N), RWORK(LENW), X(N)
      INTEGER IA(NELT), IWORK(LENIW), JA(NELT)
C     .. Local Scalars ..
      INTEGER LOCAP, LOCCSA, LOCDIN, LOCDZ, LOCEMA, LOCIW, LOCP, LOCR,
     +        LOCW, LOCZ
C     .. External Subroutines ..
      EXTERNAL SCHKW, SOMN, SS2Y, SSDI, SSDS, SSMV
C***FIRST EXECUTABLE STATEMENT  SSDOMN
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
      LOCAP = LOCP + N*(NSAVE+1)
      LOCEMA = LOCAP + N*(NSAVE+1)
      LOCDZ = LOCEMA + N*(NSAVE+1)
      LOCCSA = LOCDZ + N
      LOCW = LOCCSA + NSAVE
C
C         Check the workspace allocations.
      CALL SCHKW( 'SSDOMN', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
C
      IWORK(4) = LOCDIN
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
C
C         Compute the inverse of the diagonal of the matrix.
      CALL SSDS(N, NELT, IA, JA, A, ISYM, RWORK(LOCDIN))
C
C         Perform the Diagonally Scaled Orthomin iteration algorithm.
      CALL SOMN(N, B, X, NELT, IA, JA, A, ISYM, SSMV,
     $     SSDI, NSAVE, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT,
     $     RWORK(LOCR), RWORK(LOCZ), RWORK(LOCP), RWORK(LOCAP),
     $     RWORK(LOCEMA), RWORK(LOCDZ), RWORK(LOCCSA),
     $     RWORK, IWORK )
      RETURN
C------------- LAST LINE OF SSDOMN FOLLOWS ----------------------------
      END
