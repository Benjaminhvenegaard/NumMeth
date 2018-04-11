      SUBROUTINE DSDGMR (N, B, X, NELT, IA, JA, A, ISYM, NSAVE, ITOL,
     +   TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW)
C         The following is for optimized compilation on LLNL/LTSS Crays.
CLLL. OPTIMIZE
C     .. Parameters ..
      INTEGER LOCRB, LOCIB
      PARAMETER (LOCRB=1, LOCIB=11)
C     .. Scalar Arguments ..
      DOUBLE PRECISION ERR, TOL
      INTEGER IERR, ISYM, ITER, ITMAX, ITOL, IUNIT, LENIW, LENW, N,
     +        NELT, NSAVE
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), B(N), RWORK(LENW), X(N)
      INTEGER IA(NELT), IWORK(LENIW), JA(NELT)
C     .. Local Scalars ..
      INTEGER LOCDIN, LOCIGW, LOCIW, LOCRGW, LOCW, MYITOL
C     .. External Subroutines ..
      EXTERNAL DCHKW, DGMRES, DS2Y, DSDI, DSDS, DSMV
C***FIRST EXECUTABLE STATEMENT  DSDGMR
C
      IERR = 0
      ERR  = 0
      IF( NSAVE.LE.1 ) THEN
         IERR = 3
         RETURN
      ENDIF
C
C         Change the SLAP input matrix IA, JA, A to SLAP-Column format.
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
C
C         Set up the workspace.  We assume MAXL=KMP=NSAVE.
      LOCIGW = LOCIB
      LOCIW = LOCIGW + 20
C
      LOCDIN = LOCRB
      LOCRGW = LOCDIN + N
      LOCW = LOCRGW + 1+N*(NSAVE+6)+NSAVE*(NSAVE+3)
C
      IWORK(4) = LOCDIN
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
C
C         Check the workspace allocations.
      CALL DCHKW( 'DSDGMR', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
C
C         Compute the inverse of the diagonal of the matrix.
      CALL DSDS(N, NELT, IA, JA, A, ISYM, RWORK(LOCDIN))
C
C         Perform the Diagonally Scaled Generalized Minimum
C         Residual iteration algorithm.  The following DGMRES
C         defaults are used MAXL = KMP = NSAVE, JSCAL = 0,
C         JPRE = -1, NRMAX = ITMAX/NSAVE
      IWORK(LOCIGW  ) = NSAVE
      IWORK(LOCIGW+1) = NSAVE
      IWORK(LOCIGW+2) = 0
      IWORK(LOCIGW+3) = -1
      IWORK(LOCIGW+4) = ITMAX/NSAVE
      MYITOL = 0
C
      CALL DGMRES( N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSDI,
     $     MYITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK, RWORK,
     $     RWORK(LOCRGW), LENW-LOCRGW, IWORK(LOCIGW), 20,
     $     RWORK, IWORK )
C
      IF( ITER.GT.ITMAX ) IERR = 2
      RETURN
C------------- LAST LINE OF DSDGMR FOLLOWS ----------------------------
      END
