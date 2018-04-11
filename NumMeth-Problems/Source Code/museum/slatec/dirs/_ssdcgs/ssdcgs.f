      SUBROUTINE SSDCGS (N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
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
      INTEGER LOCDIN, LOCIW, LOCP, LOCQ, LOCR, LOCR0, LOCU, LOCV1,
     +        LOCV2, LOCW
C     .. External Subroutines ..
      EXTERNAL SCGS, SCHKW, SS2Y, SSDI, SSDS, SSMV
C***FIRST EXECUTABLE STATEMENT  SSDCGS
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
      LOCR  = LOCDIN + N
      LOCR0 = LOCR + N
      LOCP  = LOCR0 + N
      LOCQ  = LOCP + N
      LOCU  = LOCQ + N
      LOCV1 = LOCU + N
      LOCV2 = LOCV1 + N
      LOCW  = LOCV2 + N
C
C         Check the workspace allocations.
      CALL SCHKW( 'SSDCGS', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
C
      IWORK(4) = LOCDIN
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
C
C         Compute the inverse of the diagonal of the matrix.
      CALL SSDS(N, NELT, IA, JA, A, ISYM, RWORK(LOCDIN))
C
C         Perform the Diagonally Scaled
C         BiConjugate Gradient Squared algorithm.
      CALL SCGS(N, B, X, NELT, IA, JA, A, ISYM, SSMV,
     $     SSDI, ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT,
     $     RWORK(LOCR), RWORK(LOCR0), RWORK(LOCP),
     $     RWORK(LOCQ), RWORK(LOCU), RWORK(LOCV1),
     $     RWORK(LOCV2), RWORK(1), IWORK(1))
      RETURN
C------------- LAST LINE OF SSDCGS FOLLOWS ----------------------------
      END
