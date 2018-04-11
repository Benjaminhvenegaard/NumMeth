      SUBROUTINE DPSIFN (X, N, KODE, M, ANS, NZ, IERR)
      INTEGER I, IERR, J, K, KODE, M, MM, MX, N, NMAX, NN, NP, NX, NZ,
     *  FN
      INTEGER I1MACH
      DOUBLE PRECISION ANS, ARG, B, DEN, ELIM, EPS, FLN,
     * FX, RLN, RXSQ, R1M4, R1M5, S, SLOPE, T, TA, TK, TOL, TOLS, TRM,
     * TRMR, TSS, TST, TT, T1, T2, WDTOL, X, XDMLN, XDMY, XINC, XLN,
     * XM, XMIN, XQ, YINT
      DOUBLE PRECISION D1MACH
      DIMENSION B(22), TRM(22), TRMR(100), ANS(*)
      SAVE NMAX, B
      DATA NMAX /100/
C-----------------------------------------------------------------------
C             BERNOULLI NUMBERS
C-----------------------------------------------------------------------
      DATA B(1), B(2), B(3), B(4), B(5), B(6), B(7), B(8), B(9), B(10),
     * B(11), B(12), B(13), B(14), B(15), B(16), B(17), B(18), B(19),
     * B(20), B(21), B(22) /1.00000000000000000D+00,
     * -5.00000000000000000D-01,1.66666666666666667D-01,
     * -3.33333333333333333D-02,2.38095238095238095D-02,
     * -3.33333333333333333D-02,7.57575757575757576D-02,
     * -2.53113553113553114D-01,1.16666666666666667D+00,
     * -7.09215686274509804D+00,5.49711779448621554D+01,
     * -5.29124242424242424D+02,6.19212318840579710D+03,
     * -8.65802531135531136D+04,1.42551716666666667D+06,
     * -2.72982310678160920D+07,6.01580873900642368D+08,
     * -1.51163157670921569D+10,4.29614643061166667D+11,
     * -1.37116552050883328D+13,4.88332318973593167D+14,
     * -1.92965793419400681D+16/
C
C***FIRST EXECUTABLE STATEMENT  DPSIFN
      IERR = 0
      NZ=0
      IF (X.LE.0.0D0) IERR=1
      IF (N.LT.0) IERR=1
      IF (KODE.LT.1 .OR. KODE.GT.2) IERR=1
      IF (M.LT.1) IERR=1
      IF (IERR.NE.0) RETURN
      MM=M
      NX = MIN(-I1MACH(15),I1MACH(16))
      R1M5 = D1MACH(5)
      R1M4 = D1MACH(4)*0.5D0
      WDTOL = MAX(R1M4,0.5D-18)
C-----------------------------------------------------------------------
C     ELIM = APPROXIMATE EXPONENTIAL OVER AND UNDERFLOW LIMIT
C-----------------------------------------------------------------------
      ELIM = 2.302D0*(NX*R1M5-3.0D0)
      XLN = LOG(X)
   41 CONTINUE
      NN = N + MM - 1
      FN = NN
      T = (FN+1)*XLN
C-----------------------------------------------------------------------
C     OVERFLOW AND UNDERFLOW TEST FOR SMALL AND LARGE X
C-----------------------------------------------------------------------
      IF (ABS(T).GT.ELIM) GO TO 290
      IF (X.LT.WDTOL) GO TO 260
C-----------------------------------------------------------------------
C     COMPUTE XMIN AND THE NUMBER OF TERMS OF THE SERIES, FLN+1
C-----------------------------------------------------------------------
      RLN = R1M5*I1MACH(14)
      RLN = MIN(RLN,18.06D0)
      FLN = MAX(RLN,3.0D0) - 3.0D0
      YINT = 3.50D0 + 0.40D0*FLN
      SLOPE = 0.21D0 + FLN*(0.0006038D0*FLN+0.008677D0)
      XM = YINT + SLOPE*FN
      MX = INT(XM) + 1
      XMIN = MX
      IF (N.EQ.0) GO TO 50
      XM = -2.302D0*RLN - MIN(0.0D0,XLN)
      ARG = XM/N
      ARG = MIN(0.0D0,ARG)
      EPS = EXP(ARG)
      XM = 1.0D0 - EPS
      IF (ABS(ARG).LT.1.0D-3) XM = -ARG
      FLN = X*XM/EPS
      XM = XMIN - X
      IF (XM.GT.7.0D0 .AND. FLN.LT.15.0D0) GO TO 200
   50 CONTINUE
      XDMY = X
      XDMLN = XLN
      XINC = 0.0D0
      IF (X.GE.XMIN) GO TO 60
      NX = INT(X)
      XINC = XMIN - NX
      XDMY = X + XINC
      XDMLN = LOG(XDMY)
   60 CONTINUE
C-----------------------------------------------------------------------
C     GENERATE W(N+MM-1,X) BY THE ASYMPTOTIC EXPANSION
C-----------------------------------------------------------------------
      T = FN*XDMLN
      T1 = XDMLN + XDMLN
      T2 = T + XDMLN
      TK = MAX(ABS(T),ABS(T1),ABS(T2))
      IF (TK.GT.ELIM) GO TO 380
      TSS = EXP(-T)
      TT = 0.5D0/XDMY
      T1 = TT
      TST = WDTOL*TT
      IF (NN.NE.0) T1 = TT + 1.0D0/FN
      RXSQ = 1.0D0/(XDMY*XDMY)
      TA = 0.5D0*RXSQ
      T = (FN+1)*TA
      S = T*B(3)
      IF (ABS(S).LT.TST) GO TO 80
      TK = 2.0D0
      DO 70 K=4,22
        T = T*((TK+FN+1)/(TK+1.0D0))*((TK+FN)/(TK+2.0D0))*RXSQ
        TRM(K) = T*B(K)
        IF (ABS(TRM(K)).LT.TST) GO TO 80
        S = S + TRM(K)
        TK = TK + 2.0D0
   70 CONTINUE
   80 CONTINUE
      S = (S+T1)*TSS
      IF (XINC.EQ.0.0D0) GO TO 100
C-----------------------------------------------------------------------
C     BACKWARD RECUR FROM XDMY TO X
C-----------------------------------------------------------------------
      NX = INT(XINC)
      NP = NN + 1
      IF (NX.GT.NMAX) GO TO 390
      IF (NN.EQ.0) GO TO 160
      XM = XINC - 1.0D0
      FX = X + XM
C-----------------------------------------------------------------------
C     THIS LOOP SHOULD NOT BE CHANGED. FX IS ACCURATE WHEN X IS SMALL
C-----------------------------------------------------------------------
      DO 90 I=1,NX
        TRMR(I) = FX**(-NP)
        S = S + TRMR(I)
        XM = XM - 1.0D0
        FX = X + XM
   90 CONTINUE
  100 CONTINUE
      ANS(MM) = S
      IF (FN.EQ.0) GO TO 180
C-----------------------------------------------------------------------
C     GENERATE LOWER DERIVATIVES, J.LT.N+MM-1
C-----------------------------------------------------------------------
      IF (MM.EQ.1) RETURN
      DO 150 J=2,MM
        FN = FN - 1
        TSS = TSS*XDMY
        T1 = TT
        IF (FN.NE.0) T1 = TT + 1.0D0/FN
        T = (FN+1)*TA
        S = T*B(3)
        IF (ABS(S).LT.TST) GO TO 120
        TK = 4 + FN
        DO 110 K=4,22
          TRM(K) = TRM(K)*(FN+1)/TK
          IF (ABS(TRM(K)).LT.TST) GO TO 120
          S = S + TRM(K)
          TK = TK + 2.0D0
  110   CONTINUE
  120   CONTINUE
        S = (S+T1)*TSS
        IF (XINC.EQ.0.0D0) GO TO 140
        IF (FN.EQ.0) GO TO 160
        XM = XINC - 1.0D0
        FX = X + XM
        DO 130 I=1,NX
          TRMR(I) = TRMR(I)*FX
          S = S + TRMR(I)
          XM = XM - 1.0D0
          FX = X + XM
  130   CONTINUE
  140   CONTINUE
        MX = MM - J + 1
        ANS(MX) = S
        IF (FN.EQ.0) GO TO 180
  150 CONTINUE
      RETURN
C-----------------------------------------------------------------------
C     RECURSION FOR N = 0
C-----------------------------------------------------------------------
  160 CONTINUE
      DO 170 I=1,NX
        S = S + 1.0D0/(X+NX-I)
  170 CONTINUE
  180 CONTINUE
      IF (KODE.EQ.2) GO TO 190
      ANS(1) = S - XDMLN
      RETURN
  190 CONTINUE
      IF (XDMY.EQ.X) RETURN
      XQ = XDMY/X
      ANS(1) = S - LOG(XQ)
      RETURN
C-----------------------------------------------------------------------
C     COMPUTE BY SERIES (X+K)**(-(N+1)) , K=0,1,2,...
C-----------------------------------------------------------------------
  200 CONTINUE
      NN = INT(FLN) + 1
      NP = N + 1
      T1 = (N+1)*XLN
      T = EXP(-T1)
      S = T
      DEN = X
      DO 210 I=1,NN
        DEN = DEN + 1.0D0
        TRM(I) = DEN**(-NP)
        S = S + TRM(I)
  210 CONTINUE
      ANS(1) = S
      IF (N.NE.0) GO TO 220
      IF (KODE.EQ.2) ANS(1) = S + XLN
  220 CONTINUE
      IF (MM.EQ.1) RETURN
C-----------------------------------------------------------------------
C     GENERATE HIGHER DERIVATIVES, J.GT.N
C-----------------------------------------------------------------------
      TOL = WDTOL/5.0D0
      DO 250 J=2,MM
        T = T/X
        S = T
        TOLS = T*TOL
        DEN = X
        DO 230 I=1,NN
          DEN = DEN + 1.0D0
          TRM(I) = TRM(I)/DEN
          S = S + TRM(I)
          IF (TRM(I).LT.TOLS) GO TO 240
  230   CONTINUE
  240   CONTINUE
        ANS(J) = S
  250 CONTINUE
      RETURN
C-----------------------------------------------------------------------
C     SMALL X.LT.UNIT ROUND OFF
C-----------------------------------------------------------------------
  260 CONTINUE
      ANS(1) = X**(-N-1)
      IF (MM.EQ.1) GO TO 280
      K = 1
      DO 270 I=2,MM
        ANS(K+1) = ANS(K)/X
        K = K + 1
  270 CONTINUE
  280 CONTINUE
      IF (N.NE.0) RETURN
      IF (KODE.EQ.2) ANS(1) = ANS(1) + XLN
      RETURN
  290 CONTINUE
      IF (T.GT.0.0D0) GO TO 380
      NZ=0
      IERR=2
      RETURN
  380 CONTINUE
      NZ=NZ+1
      ANS(MM)=0.0D0
      MM=MM-1
      IF (MM.EQ.0) RETURN
      GO TO 41
  390 CONTINUE
      NZ=0
      IERR=3
      RETURN
      END
