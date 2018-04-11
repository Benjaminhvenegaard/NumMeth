      SUBROUTINE DBKISR (X, N, SUM, IERR)
      INTEGER I, IERR, K, KK, KKN, K1, N, NP
      DOUBLE PRECISION AK, ATOL, BK, C, FK, FN, HX, HXS, POL, PR, SUM,
     * TKP, TOL, TRM, X, XLN
      DOUBLE PRECISION DPSIXN, D1MACH
      DIMENSION C(2)
      SAVE C
C
      DATA C(1), C(2) /1.57079632679489662D+00,1.0D0/
C***FIRST EXECUTABLE STATEMENT  DBKISR
      IERR=0
      TOL = MAX(D1MACH(4),1.0D-18)
      IF (X.LT.TOL) GO TO 50
      PR = 1.0D0
      POL = 0.0D0
      IF (N.EQ.0) GO TO 20
      DO 10 I=1,N
        POL = -POL*X + C(I)
        PR = PR*X/I
   10 CONTINUE
   20 CONTINUE
      HX = X*0.5D0
      HXS = HX*HX
      XLN = LOG(HX)
      NP = N + 1
      TKP = 3.0D0
      FK = 2.0D0
      FN = N
      BK = 4.0D0
      AK = 2.0D0/((FN+1.0D0)*(FN+2.0D0))
      SUM = AK*(DPSIXN(N+3)-DPSIXN(3)+DPSIXN(2)-XLN)
      ATOL = SUM*TOL*0.75D0
      DO 30 K=2,20
        AK = AK*(HXS/BK)*((TKP+1.0D0)/(TKP+FN+1.0D0))*(TKP/(TKP+FN))
        K1 = K + 1
        KK = K1 + K
        KKN = KK + N
        TRM = (DPSIXN(K1)+DPSIXN(KKN)-DPSIXN(KK)-XLN)*AK
        SUM = SUM + TRM
        IF (ABS(TRM).LE.ATOL) GO TO 40
        TKP = TKP + 2.0D0
        BK = BK + TKP
        FK = FK + 1.0D0
   30 CONTINUE
      GO TO 80
   40 CONTINUE
      SUM = (SUM*HXS+DPSIXN(NP)-XLN)*PR
      IF (N.EQ.1) SUM = -SUM
      SUM = POL + SUM
      RETURN
C-----------------------------------------------------------------------
C     SMALL X CASE, X.LT.WORD TOLERANCE
C-----------------------------------------------------------------------
   50 CONTINUE
      IF (N.GT.0) GO TO 60
      HX = X*0.5D0
      SUM = DPSIXN(1) - LOG(HX)
      RETURN
   60 CONTINUE
      SUM = C(N)
      RETURN
   80 CONTINUE
      IERR=2
      RETURN
      END
