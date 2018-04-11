      SUBROUTINE DPIGMR (N, R0, SR, SZ, JSCAL, MAXL, MAXLP1, KMP, NRSTS,
     +   JPRE, MATVEC, MSOLVE, NMSL, Z, V, HES, Q, LGMR, RPAR, IPAR, WK,
     +   DL, RHOL, NRMAX, B, BNRM, X, XL, ITOL, TOL, NELT, IA, JA, A,
     +   ISYM, IUNIT, IFLAG, ERR)
C         The following is for optimized compilation on LLNL/LTSS Crays.
CLLL. OPTIMIZE
C     .. Scalar Arguments ..
      DOUBLE PRECISION BNRM, ERR, RHOL, TOL
      INTEGER IFLAG, ISYM, ITOL, IUNIT, JPRE, JSCAL, KMP, LGMR, MAXL,
     +        MAXLP1, N, NELT, NMSL, NRMAX, NRSTS
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), B(*), DL(*), HES(MAXLP1,*), Q(*), R0(*),
     +                 RPAR(*), SR(*), SZ(*), V(N,*), WK(*), X(*),
     +                 XL(*), Z(*)
      INTEGER IA(NELT), IPAR(*), JA(NELT)
C     .. Subroutine Arguments ..
      EXTERNAL MATVEC, MSOLVE
C     .. Local Scalars ..
      DOUBLE PRECISION C, DLNRM, PROD, R0NRM, RHO, S, SNORMW, TEM
      INTEGER I, I2, INFO, IP1, ITER, ITMAX, J, K, LL, LLP1
C     .. External Functions ..
      DOUBLE PRECISION DNRM2
      INTEGER ISDGMR
      EXTERNAL DNRM2, ISDGMR
C     .. External Subroutines ..
      EXTERNAL DAXPY, DCOPY, DHELS, DHEQR, DORTH, DRLCAL, DSCAL
C     .. Intrinsic Functions ..
      INTRINSIC ABS
C***FIRST EXECUTABLE STATEMENT  DPIGMR
C
C         Zero out the Z array.
C
      DO 5 I = 1,N
         Z(I) = 0
 5    CONTINUE
C
      IFLAG = 0
      LGMR = 0
      NMSL = 0
C         Load ITMAX, the maximum number of iterations.
      ITMAX =(NRMAX+1)*MAXL
C   -------------------------------------------------------------------
C         The initial residual is the vector R0.
C         Apply left precon. if JPRE < 0 and this is not a restart.
C         Apply scaling to R0 if JSCAL = 2 or 3.
C   -------------------------------------------------------------------
      IF ((JPRE .LT. 0) .AND.(NRSTS .EQ. 0)) THEN
         CALL DCOPY(N, R0, 1, WK, 1)
         CALL MSOLVE(N, WK, R0, NELT, IA, JA, A, ISYM, RPAR, IPAR)
         NMSL = NMSL + 1
      ENDIF
      IF (((JSCAL.EQ.2) .OR.(JSCAL.EQ.3)) .AND.(NRSTS.EQ.0)) THEN
         DO 10 I = 1,N
            V(I,1) = R0(I)*SR(I)
 10      CONTINUE
      ELSE
         DO 20 I = 1,N
            V(I,1) = R0(I)
 20      CONTINUE
      ENDIF
      R0NRM = DNRM2(N, V, 1)
      ITER = NRSTS*MAXL
C
C         Call stopping routine ISDGMR.
C
      IF (ISDGMR(N, B, X, XL, NELT, IA, JA, A, ISYM, MSOLVE,
     $    NMSL, ITOL, TOL, ITMAX, ITER, ERR, IUNIT, V(1,1), Z, WK,
     $    RPAR, IPAR, R0NRM, BNRM, SR, SZ, JSCAL,
     $    KMP, LGMR, MAXL, MAXLP1, V, Q, SNORMW, PROD, R0NRM,
     $    HES, JPRE) .NE. 0) RETURN
      TEM = 1.0D0/R0NRM
      CALL DSCAL(N, TEM, V(1,1), 1)
C
C         Zero out the HES array.
C
      DO 50 J = 1,MAXL
         DO 40 I = 1,MAXLP1
            HES(I,J) = 0
 40      CONTINUE
 50   CONTINUE
C   -------------------------------------------------------------------
C         Main loop to compute the vectors V(*,2) to V(*,MAXL).
C         The running product PROD is needed for the convergence test.
C   -------------------------------------------------------------------
      PROD = 1
      DO 90 LL = 1,MAXL
         LGMR = LL
C   -------------------------------------------------------------------
C        Unscale  the  current V(LL)  and store  in WK.  Call routine
C        MSOLVE    to   compute(M-inverse)*WK,   where    M   is  the
C        preconditioner matrix.  Save the answer in Z.   Call routine
C        MATVEC to compute  VNEW  = A*Z,  where  A is  the the system
C        matrix.  save the answer in  V(LL+1).  Scale V(LL+1).   Call
C        routine DORTH  to  orthogonalize the    new vector VNEW   =
C        V(*,LL+1).  Call routine DHEQR to update the factors of HES.
C   -------------------------------------------------------------------
        IF ((JSCAL .EQ. 1) .OR.(JSCAL .EQ. 3)) THEN
           DO 60 I = 1,N
              WK(I) = V(I,LL)/SZ(I)
 60        CONTINUE
        ELSE
           CALL DCOPY(N, V(1,LL), 1, WK, 1)
        ENDIF
        IF (JPRE .GT. 0) THEN
           CALL MSOLVE(N, WK, Z, NELT, IA, JA, A, ISYM, RPAR, IPAR)
           NMSL = NMSL + 1
           CALL MATVEC(N, Z, V(1,LL+1), NELT, IA, JA, A, ISYM)
        ELSE
           CALL MATVEC(N, WK, V(1,LL+1), NELT, IA, JA, A, ISYM)
        ENDIF
        IF (JPRE .LT. 0) THEN
           CALL DCOPY(N, V(1,LL+1), 1, WK, 1)
           CALL MSOLVE(N,WK,V(1,LL+1),NELT,IA,JA,A,ISYM,RPAR,IPAR)
           NMSL = NMSL + 1
        ENDIF
        IF ((JSCAL .EQ. 2) .OR.(JSCAL .EQ. 3)) THEN
           DO 65 I = 1,N
              V(I,LL+1) = V(I,LL+1)*SR(I)
 65        CONTINUE
        ENDIF
        CALL DORTH(V(1,LL+1), V, HES, N, LL, MAXLP1, KMP, SNORMW)
        HES(LL+1,LL) = SNORMW
        CALL DHEQR(HES, MAXLP1, LL, Q, INFO, LL)
        IF (INFO .EQ. LL) GO TO 120
C   -------------------------------------------------------------------
C         Update RHO, the estimate of the norm of the residual R0-A*ZL.
C         If KMP <  MAXL, then the vectors V(*,1),...,V(*,LL+1) are not
C         necessarily orthogonal for LL > KMP.  The vector DL must then
C         be computed, and its norm used in the calculation of RHO.
C   -------------------------------------------------------------------
        PROD = PROD*Q(2*LL)
        RHO = ABS(PROD*R0NRM)
        IF ((LL.GT.KMP) .AND.(KMP.LT.MAXL)) THEN
           IF (LL .EQ. KMP+1) THEN
              CALL DCOPY(N, V(1,1), 1, DL, 1)
              DO 75 I = 1,KMP
                 IP1 = I + 1
                 I2 = I*2
                 S = Q(I2)
                 C = Q(I2-1)
                 DO 70 K = 1,N
                    DL(K) = S*DL(K) + C*V(K,IP1)
 70              CONTINUE
 75           CONTINUE
           ENDIF
           S = Q(2*LL)
           C = Q(2*LL-1)/SNORMW
           LLP1 = LL + 1
           DO 80 K = 1,N
              DL(K) = S*DL(K) + C*V(K,LLP1)
 80        CONTINUE
           DLNRM = DNRM2(N, DL, 1)
           RHO = RHO*DLNRM
        ENDIF
        RHOL = RHO
C   -------------------------------------------------------------------
C         Test for convergence.  If passed, compute approximation ZL.
C         If failed and LL < MAXL, then continue iterating.
C   -------------------------------------------------------------------
        ITER = NRSTS*MAXL + LGMR
        IF (ISDGMR(N, B, X, XL, NELT, IA, JA, A, ISYM, MSOLVE,
     $      NMSL, ITOL, TOL, ITMAX, ITER, ERR, IUNIT, DL, Z, WK,
     $      RPAR, IPAR, RHOL, BNRM, SR, SZ, JSCAL,
     $      KMP, LGMR, MAXL, MAXLP1, V, Q, SNORMW, PROD, R0NRM,
     $      HES, JPRE) .NE. 0) GO TO 200
        IF (LL .EQ. MAXL) GO TO 100
C   -------------------------------------------------------------------
C         Rescale so that the norm of V(1,LL+1) is one.
C   -------------------------------------------------------------------
        TEM = 1.0D0/SNORMW
        CALL DSCAL(N, TEM, V(1,LL+1), 1)
 90   CONTINUE
 100  CONTINUE
      IF (RHO .LT. R0NRM) GO TO 150
 120  CONTINUE
      IFLAG = 2
C
C         Load approximate solution with zero.
C
      DO 130 I = 1,N
         Z(I) = 0
 130  CONTINUE
      RETURN
 150  IFLAG = 1
C
C         Tolerance not met, but residual norm reduced.
C
      IF (NRMAX .GT. 0) THEN
C
C        If performing restarting (NRMAX > 0)  calculate the residual
C        vector RL and  store it in the DL  array.  If the incomplete
C        version is being used (KMP < MAXL) then DL has  already been
C        calculated up to a scaling factor.   Use DRLCAL to calculate
C        the scaled residual vector.
C
         CALL DRLCAL(N, KMP, MAXL, MAXL, V, Q, DL, SNORMW, PROD,
     $        R0NRM)
      ENDIF
C   -------------------------------------------------------------------
C         Compute the approximation ZL to the solution.  Since the
C         vector Z was used as workspace, and the initial guess
C         of the linear iteration is zero, Z must be reset to zero.
C   -------------------------------------------------------------------
 200  CONTINUE
      LL = LGMR
      LLP1 = LL + 1
      DO 210 K = 1,LLP1
         R0(K) = 0
 210  CONTINUE
      R0(1) = R0NRM
      CALL DHELS(HES, MAXLP1, LL, Q, R0)
      DO 220 K = 1,N
         Z(K) = 0
 220  CONTINUE
      DO 230 I = 1,LL
         CALL DAXPY(N, R0(I), V(1,I), 1, Z, 1)
 230  CONTINUE
      IF ((JSCAL .EQ. 1) .OR.(JSCAL .EQ. 3)) THEN
         DO 240 I = 1,N
            Z(I) = Z(I)/SZ(I)
 240     CONTINUE
      ENDIF
      IF (JPRE .GT. 0) THEN
         CALL DCOPY(N, Z, 1, WK, 1)
         CALL MSOLVE(N, WK, Z, NELT, IA, JA, A, ISYM, RPAR, IPAR)
         NMSL = NMSL + 1
      ENDIF
      RETURN
C------------- LAST LINE OF DPIGMR FOLLOWS ----------------------------
      END
