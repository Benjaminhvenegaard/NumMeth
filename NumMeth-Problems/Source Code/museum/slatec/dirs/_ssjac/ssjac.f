      SUBROUTINE SSJAC (N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
     +   ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW)
C     .. Parameters ..
      INTEGER LOCRB, LOCIB
      PARAMETER (LOCRB=1, LOCIB=11)
C     .. Scalar Arguments ..
      REAL ERR, TOL
      INTEGER IERR, ISYM, ITER, ITMAX, ITOL, IUNIT, LENIW, LENW, N, NELT
C     .. Array Arguments ..
      REAL A(NELT), B(N), RWORK(LENW), X(N)
      INTEGER IA(NELT), IWORK(LENIW), JA(NELT)
C     .. Local Scalars ..
      INTEGER LOCD, LOCDZ, LOCIW, LOCR, LOCW, LOCZ
C     .. External Subroutines ..
      EXTERNAL SCHKW, SIR, SS2Y, SSDI, SSDS, SSMV
C***FIRST EXECUTABLE STATEMENT  SSJAC
C
      IERR = 0
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      ENDIF
      LOCIW = LOCIB
      LOCD = LOCRB
      LOCR = LOCD + N
      LOCZ = LOCR + N
      LOCDZ = LOCZ + N
      LOCW = LOCDZ + N
C
C         Check the workspace allocations.
      CALL SCHKW( 'SSJAC', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
C
      IWORK(4) = LOCD
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
C
C         Convert to SLAP column format.
      CALL SS2Y(N, NELT, IA, JA, A, ISYM )
C
C         Compute the inverse of the diagonal of the matrix.  This
C         will be used as the preconditioner.
      CALL SSDS(N, NELT, IA, JA, A, ISYM, RWORK(LOCD))
C
C         Set up the work array and perform the iterative refinement.
      CALL SIR(N, B, X, NELT, IA, JA, A, ISYM, SSMV, SSDI, ITOL, TOL,
     $     ITMAX, ITER, ERR, IERR, IUNIT, RWORK(LOCR), RWORK(LOCZ),
     $     RWORK(LOCDZ), RWORK, IWORK )
      RETURN
C------------- LAST LINE OF SSJAC FOLLOWS -----------------------------
      END
