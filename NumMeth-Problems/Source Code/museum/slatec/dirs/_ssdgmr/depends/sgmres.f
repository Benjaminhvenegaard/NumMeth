      SUBROUTINE SGMRES (N, B, X, NELT, IA, JA, A, ISYM, MATVEC, MSOLVE,
     +   ITOL, TOL, ITMAX, ITER, ERR, IERR, IUNIT, SB, SX, RGWK, LRGW,
     +   IGWK, LIGW, RWORK, IWORK)
C         The following is for optimized compilation on LLNL/LTSS Crays.
CLLL. OPTIMIZE
C     .. Scalar Arguments ..
      REAL ERR, TOL
      INTEGER IERR, ISYM, ITER, ITMAX, ITOL, IUNIT, LIGW, LRGW, N, NELT
C     .. Array Arguments ..
      REAL A(NELT), B(N), RGWK(LRGW), RWORK(*), SB(N), SX(N), X(N)
      INTEGER IA(NELT), IGWK(LIGW), IWORK(*), JA(NELT)
C     .. Subroutine Arguments ..
      EXTERNAL MATVEC, MSOLVE
C     .. Local Scalars ..
      REAL BNRM, RHOL, SUM
      INTEGER I, IFLAG, JPRE, JSCAL, KMP, LDL, LGMR, LHES, LQ, LR, LV,
     +        LW, LXL, LZ, LZM1, MAXL, MAXLP1, NMS, NMSL, NRMAX, NRSTS
C     .. External Functions ..
      REAL R1MACH, SNRM2
      EXTERNAL R1MACH, SNRM2
C     .. External Subroutines ..
      EXTERNAL SCOPY, SPIGMR
C     .. Intrinsic Functions ..
      INTRINSIC SQRT
C***FIRST EXECUTABLE STATEMENT  SGMRES
      IERR = 0
C   ------------------------------------------------------------------
C         Load method parameters with user values or defaults.
C   ------------------------------------------------------------------
      MAXL = IGWK(1)
      IF (MAXL .EQ. 0) MAXL = 10
      IF (MAXL .GT. N) MAXL = N
      KMP = IGWK(2)
      IF (KMP .EQ. 0) KMP = MAXL
      IF (KMP .GT. MAXL) KMP = MAXL
      JSCAL = IGWK(3)
      JPRE = IGWK(4)
C         Check for valid value of ITOL.
      IF( (ITOL.LT.0) .OR. ((ITOL.GT.3).AND.(ITOL.NE.11)) ) GOTO 650
C         Check for consistent values of ITOL and JPRE.
      IF( ITOL.EQ.1 .AND. JPRE.LT.0 ) GOTO 650
      IF( ITOL.EQ.2 .AND. JPRE.GE.0 ) GOTO 650
      NRMAX = IGWK(5)
      IF( NRMAX.EQ.0 ) NRMAX = 10
C         If NRMAX .eq. -1, then set NRMAX = 0 to turn off restarting.
      IF( NRMAX.EQ.-1 ) NRMAX = 0
C         If input value of TOL is zero, set it to its default value.
      IF( TOL.EQ.0.0E0 ) TOL = 500*R1MACH(3)
C
C         Initialize counters.
      ITER = 0
      NMS = 0
      NRSTS = 0
C   ------------------------------------------------------------------
C         Form work array segment pointers.
C   ------------------------------------------------------------------
      MAXLP1 = MAXL + 1
      LV = 1
      LR = LV + N*MAXLP1
      LHES = LR + N + 1
      LQ = LHES + MAXL*MAXLP1
      LDL = LQ + 2*MAXL
      LW = LDL + N
      LXL = LW + N
      LZ = LXL + N
C
C         Load IGWK(6) with required minimum length of the RGWK array.
      IGWK(6) = LZ + N - 1
      IF( LZ+N-1.GT.LRGW ) GOTO 640
C   ------------------------------------------------------------------
C         Calculate scaled-preconditioned norm of RHS vector b.
C   ------------------------------------------------------------------
      IF (JPRE .LT. 0) THEN
         CALL MSOLVE(N, B, RGWK(LR), NELT, IA, JA, A, ISYM,
     $        RWORK, IWORK)
         NMS = NMS + 1
      ELSE
         CALL SCOPY(N, B, 1, RGWK(LR), 1)
      ENDIF
      IF( JSCAL.EQ.2 .OR. JSCAL.EQ.3 ) THEN
         SUM = 0
         DO 10 I = 1,N
            SUM = SUM + (RGWK(LR-1+I)*SB(I))**2
 10      CONTINUE
         BNRM = SQRT(SUM)
      ELSE
         BNRM = SNRM2(N,RGWK(LR),1)
      ENDIF
C   ------------------------------------------------------------------
C         Calculate initial residual.
C   ------------------------------------------------------------------
      CALL MATVEC(N, X, RGWK(LR), NELT, IA, JA, A, ISYM)
      DO 50 I = 1,N
         RGWK(LR-1+I) = B(I) - RGWK(LR-1+I)
 50   CONTINUE
C   ------------------------------------------------------------------
C         If performing restarting, then load the residual into the
C         correct location in the RGWK array.
C   ------------------------------------------------------------------
 100  CONTINUE
      IF( NRSTS.GT.NRMAX ) GOTO 610
      IF( NRSTS.GT.0 ) THEN
C         Copy the current residual to a different location in the RGWK
C         array.
         CALL SCOPY(N, RGWK(LDL), 1, RGWK(LR), 1)
      ENDIF
C   ------------------------------------------------------------------
C         Use the SPIGMR algorithm to solve the linear system A*Z = R.
C   ------------------------------------------------------------------
      CALL SPIGMR(N, RGWK(LR), SB, SX, JSCAL, MAXL, MAXLP1, KMP,
     $       NRSTS, JPRE, MATVEC, MSOLVE, NMSL, RGWK(LZ), RGWK(LV),
     $       RGWK(LHES), RGWK(LQ), LGMR, RWORK, IWORK, RGWK(LW),
     $       RGWK(LDL), RHOL, NRMAX, B, BNRM, X, RGWK(LXL), ITOL,
     $       TOL, NELT, IA, JA, A, ISYM, IUNIT, IFLAG, ERR)
      ITER = ITER + LGMR
      NMS = NMS + NMSL
C
C         Increment X by the current approximate solution Z of A*Z = R.
C
      LZM1 = LZ - 1
      DO 110 I = 1,N
         X(I) = X(I) + RGWK(LZM1+I)
 110  CONTINUE
      IF( IFLAG.EQ.0 ) GOTO 600
      IF( IFLAG.EQ.1 ) THEN
         NRSTS = NRSTS + 1
         GOTO 100
      ENDIF
      IF( IFLAG.EQ.2 ) GOTO 620
C   ------------------------------------------------------------------
C         All returns are made through this section.
C   ------------------------------------------------------------------
C         The iteration has converged.
C
 600  CONTINUE
      IGWK(7) = NMS
      RGWK(1) = RHOL
      IERR = 0
      RETURN
C
C         Max number((NRMAX+1)*MAXL) of linear iterations performed.
 610  CONTINUE
      IGWK(7) = NMS
      RGWK(1) = RHOL
      IERR = 1
      RETURN
C
C         GMRES failed to reduce last residual in MAXL iterations.
C         The iteration has stalled.
 620  CONTINUE
      IGWK(7) = NMS
      RGWK(1) = RHOL
      IERR = 2
      RETURN
C         Error return.  Insufficient length for RGWK array.
 640  CONTINUE
      ERR = TOL
      IERR = -1
      RETURN
C         Error return.  Inconsistent ITOL and JPRE values.
 650  CONTINUE
      ERR = TOL
      IERR = -2
      RETURN
C------------- LAST LINE OF SGMRES FOLLOWS ----------------------------
      END
