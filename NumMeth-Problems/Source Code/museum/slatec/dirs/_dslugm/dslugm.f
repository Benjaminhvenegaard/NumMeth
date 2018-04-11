      SUBROUTINE DSLUGM (N, B, X, NELT, IA, JA, A, ISYM, NSAVE, ITOL,
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
      INTEGER ICOL, J, JBGN, JEND, LOCDIN, LOCIGW, LOCIL, LOCIU, LOCIW,
     +        LOCJL, LOCJU, LOCL, LOCNC, LOCNR, LOCRGW, LOCU, LOCW,
     +        MYITOL, NL, NU
C     .. External Subroutines ..
      EXTERNAL DCHKW, DGMRES, DS2Y, DSILUS, DSLUI, DSMV
C***FIRST EXECUTABLE STATEMENT  DSLUGM
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
C         Count number of Non-Zero elements preconditioner ILU matrix.
C         Then set up the work arrays.  We assume MAXL=KMP=NSAVE.
      NL = 0
      NU = 0
      DO 20 ICOL = 1, N
C         Don't count diagonal.
         JBGN = JA(ICOL)+1
         JEND = JA(ICOL+1)-1
         IF( JBGN.LE.JEND ) THEN
CVD$ NOVECTOR
            DO 10 J = JBGN, JEND
               IF( IA(J).GT.ICOL ) THEN
                  NL = NL + 1
                  IF( ISYM.NE.0 ) NU = NU + 1
               ELSE
                  NU = NU + 1
               ENDIF
 10         CONTINUE
         ENDIF
 20   CONTINUE
C
      LOCIGW = LOCIB
      LOCIL = LOCIGW + 20
      LOCJL = LOCIL + N+1
      LOCIU = LOCJL + NL
      LOCJU = LOCIU + NU
      LOCNR = LOCJU + N+1
      LOCNC = LOCNR + N
      LOCIW = LOCNC + N
C
      LOCL = LOCRB
      LOCDIN = LOCL + NL
      LOCU = LOCDIN + N
      LOCRGW = LOCU + NU
      LOCW = LOCRGW + 1+N*(NSAVE+6)+NSAVE*(NSAVE+3)
C
C         Check the workspace allocations.
      CALL DCHKW( 'DSLUGM', LOCIW, LENIW, LOCW, LENW, IERR, ITER, ERR )
      IF( IERR.NE.0 ) RETURN
C
      IWORK(1) = LOCIL
      IWORK(2) = LOCJL
      IWORK(3) = LOCIU
      IWORK(4) = LOCJU
      IWORK(5) = LOCL
      IWORK(6) = LOCDIN
      IWORK(7) = LOCU
      IWORK(9) = LOCIW
      IWORK(10) = LOCW
C
C         Compute the Incomplete LU decomposition.
      CALL DSILUS( N, NELT, IA, JA, A, ISYM, NL, IWORK(LOCIL),
     $     IWORK(LOCJL), RWORK(LOCL), RWORK(LOCDIN), NU, IWORK(LOCIU),
     $     IWORK(LOCJU), RWORK(LOCU), IWORK(LOCNR), IWORK(LOCNC) )
C
C         Perform the Incomplete LU Preconditioned Generalized Minimum
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
      CALL DGMRES( N, B, X, NELT, IA, JA, A, ISYM, DSMV, DSLUI,
     $     MYITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, RWORK, RWORK,
     $     RWORK(LOCRGW), LENW-LOCRGW, IWORK(LOCIGW), 20,
     $     RWORK, IWORK )
C
      IF( ITER.GT.ITMAX ) IERR = 2
      RETURN
C------------- LAST LINE OF DSLUGM FOLLOWS ----------------------------
      END
