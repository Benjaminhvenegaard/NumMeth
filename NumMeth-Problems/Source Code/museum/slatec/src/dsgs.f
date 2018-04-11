      SUBROUTINE DSGS (N, B, X, NELT, IA, JA, A, ISYM, ITOL, TOL, ITMAX,
     +   ITER, ERR, IERR, IUNIT, RWORK, LENW, IWORK, LENIW)
C     .. Parameters ..
      INTEGER LOCRB, LOCIB
      PARAMETER (LOCRB=1, LOCIB=11)
C     .. Scalar Arguments ..
      DOUBLE PRECISION ERR, TOL
      INTEGER IERR, ISYM, ITER, ITMAX, ITOL, IUNIT, LENIW, LENW, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(N), B(N), RWORK(*), X(N)
      INTEGER IA(NELT), IWORK(10), JA(NELT)
C     .. Local Scalars ..
      INTEGER ICOL, J, JBGN, JEND, LOCDZ, LOCEL, LOCIEL, LOCIW, LOCJEL,
     +        LOCR, LOCW, LOCZ, NL
C     .. External Subroutines ..
      EXTERNAL DCHKW, DIR, DS2LT, DS2Y, DSLI, DSMV
C***FIRST EXECUTABLE STATEMENT  DSGS
C
      IF( N.LT.1 .OR. NELT.LT.1 ) THEN
         IERR = 3
         RETURN
      ENDIF
C
C         Modify the SLAP matrix data structure to YSMP-Column.
      CALL DS2Y( N, NELT, IA, JA, A, ISYM )
C
C         Count number of elements in lower triangle of the matrix.
      IF( ISYM.EQ.0 ) THEN
         NL = 0
         DO 20 ICOL = 1, N
            JBGN = JA(ICOL)
            JEND = JA(ICOL+1)-1
            DO 10 J = JBGN, JEND
               IF( IA(J).GE.ICOL ) NL = NL + 1
 10         CONTINUE
 20      CONTINUE
      ELSE
         NL = JA(N+1)-1
      ENDIF
C
C         Set up the work arrays.  Then store the lower triangle of
C         the matrix.
C
      LOCJEL = LOCIB
      LOCIEL = LOCJEL + N+1
      LOCIW = LOCIEL + NL
C
      LOCEL = LOCRB
      LOCR = LOCEL + NL
      LOCZ = LOCR + N
      LOCDZ = LOCZ + N
      LOCW = LOCDZ + N
C
C         Check the workspace allocations.
      CALL DCHKW( 'DSGS', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
C
      IWORK(1) = NL
      IWORK(2) = LOCIEL
      IWORK(3) = LOCJEL
      IWORK(4) = LOCEL
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
C
      CALL DS2LT( N, NELT, IA, JA, A, ISYM, NL, IWORK(LOCIEL),
     $     IWORK(LOCJEL), RWORK(LOCEL) )
C
C         Call iterative refinement routine.
      CALL DIR(N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSLI,
     $     ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK(LOCR),
     $     RWORK(LOCZ), RWORK(LOCDZ), RWORK, IWORK )
C
C         Set the amount of Integer and Double Precision Workspace used.
      IWORK(9) = LOCIW+N+NELT
      IWORK(10) = LOCW+NELT
      RETURN
C------------- LAST LINE OF DSGS FOLLOWS ------------------------------
      END
