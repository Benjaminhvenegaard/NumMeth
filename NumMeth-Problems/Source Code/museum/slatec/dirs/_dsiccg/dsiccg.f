      SUBROUTINE DSICCG (N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL,
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
      INTEGER LOCDIN, LOCDZ, LOCEL, LOCIEL, LOCIW, LOCJEL, LOCP, LOCR,
     +        LOCW, LOCZ, NL
      CHARACTER XERN1*8
C     .. External Subroutines ..
      EXTERNAL DCG, DCHKW, DS2Y, DSICS, DSLLTI, DSMV, XERMSG
C***FIRST EXECUTABLE STATEMENT  DSICCG
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
C         Count number of elements in lower triangle of the matrix.
C         Then set up the work arrays.
      IF( ISYM.EQ.0 ) THEN
         NL = (NELT + N)/2
      ELSE
         NL = NELT
      ENDIF
C
      LOCJEL = LOCIB
      LOCIEL = LOCJEL + NL
      LOCIW = LOCIEL + N + 1
C
      LOCEL = LOCRB
      LOCDIN = LOCEL + NL
      LOCR = LOCDIN + N
      LOCZ = LOCR + N
      LOCP = LOCZ + N
      LOCDZ = LOCP + N
      LOCW = LOCDZ + N
C
C         Check the workspace allocations.
      CALL DCHKW( 'DSICCG', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
C
      IWORK(1) = NL
      IWORK(2) = LOCJEL
      IWORK(3) = LOCIEL
      IWORK(4) = LOCEL
      IWORK(5) = LOCDIN
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
C
C         Compute the Incomplete Cholesky decomposition.
C
      CALL DSICS(N, NELT, IA, JA, A, ISYM, NL, IWORK(LOCIEL),
     $     IWORK(LOCJEL), RWORK(LOCEL), RWORK(LOCDIN),
     $     RWORK(LOCR), IERR )
      IF( IERR.NE.0 ) THEN
         WRITE (XERN1, '(I8)') IERR
         CALL XERMSG ('SLATEC', 'DSICCG',
     $      'IC factorization broke down on step ' // XERN1 //
     $      '.  Diagonal was set to unity and factorization proceeded.',
     $      1, 1)
         IERR = 7
      ENDIF
C
C         Do the Preconditioned Conjugate Gradient.
      CALL DCG(N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSLLTI,
     $     ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK(LOCR),
     $     RWORK(LOCZ), RWORK(LOCP), RWORK(LOCDZ), RWORK(1),
     $     IWORK(1))
      RETURN
C------------- LAST LINE OF DSICCG FOLLOWS ----------------------------
      END
