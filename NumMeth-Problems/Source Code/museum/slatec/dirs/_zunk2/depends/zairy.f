      SUBROUTINE ZAIRY (ZR, ZI, ID, KODE, AIR, AII, NZ, IERR)
C     COMPLEX AI,CONE,CSQ,CY,S1,S2,TRM1,TRM2,Z,ZTA,Z3
      DOUBLE PRECISION AA, AD, AII, AIR, AK, ALIM, ATRM, AZ, AZ3, BK,
     * CC, CK, COEF, CONEI, CONER, CSQI, CSQR, CYI, CYR, C1, C2, DIG,
     * DK, D1, D2, ELIM, FID, FNU, PTR, RL, R1M5, SFAC, STI, STR,
     * S1I, S1R, S2I, S2R, TOL, TRM1I, TRM1R, TRM2I, TRM2R, TTH, ZEROI,
     * ZEROR, ZI, ZR, ZTAI, ZTAR, Z3I, Z3R, D1MACH, ZABS, ALAZ, BB
      INTEGER ID, IERR, IFLAG, K, KODE, K1, K2, MR, NN, NZ, I1MACH
      DIMENSION CYR(1), CYI(1)
      EXTERNAL ZABS, ZEXP, ZSQRT
      DATA TTH, C1, C2, COEF /6.66666666666666667D-01,
     * 3.55028053887817240D-01,2.58819403792806799D-01,
     * 1.83776298473930683D-01/
      DATA ZEROR, ZEROI, CONER, CONEI /0.0D0,0.0D0,1.0D0,0.0D0/
C***FIRST EXECUTABLE STATEMENT  ZAIRY
      IERR = 0
      NZ=0
      IF (ID.LT.0 .OR. ID.GT.1) IERR=1
      IF (KODE.LT.1 .OR. KODE.GT.2) IERR=1
      IF (IERR.NE.0) RETURN
      AZ = ZABS(ZR,ZI)
      TOL = MAX(D1MACH(4),1.0D-18)
      FID = ID
      IF (AZ.GT.1.0D0) GO TO 70
C-----------------------------------------------------------------------
C     POWER SERIES FOR ABS(Z).LE.1.
C-----------------------------------------------------------------------
      S1R = CONER
      S1I = CONEI
      S2R = CONER
      S2I = CONEI
      IF (AZ.LT.TOL) GO TO 170
      AA = AZ*AZ
      IF (AA.LT.TOL/AZ) GO TO 40
      TRM1R = CONER
      TRM1I = CONEI
      TRM2R = CONER
      TRM2I = CONEI
      ATRM = 1.0D0
      STR = ZR*ZR - ZI*ZI
      STI = ZR*ZI + ZI*ZR
      Z3R = STR*ZR - STI*ZI
      Z3I = STR*ZI + STI*ZR
      AZ3 = AZ*AA
      AK = 2.0D0 + FID
      BK = 3.0D0 - FID - FID
      CK = 4.0D0 - FID
      DK = 3.0D0 + FID + FID
      D1 = AK*DK
      D2 = BK*CK
      AD = MIN(D1,D2)
      AK = 24.0D0 + 9.0D0*FID
      BK = 30.0D0 - 9.0D0*FID
      DO 30 K=1,25
        STR = (TRM1R*Z3R-TRM1I*Z3I)/D1
        TRM1I = (TRM1R*Z3I+TRM1I*Z3R)/D1
        TRM1R = STR
        S1R = S1R + TRM1R
        S1I = S1I + TRM1I
        STR = (TRM2R*Z3R-TRM2I*Z3I)/D2
        TRM2I = (TRM2R*Z3I+TRM2I*Z3R)/D2
        TRM2R = STR
        S2R = S2R + TRM2R
        S2I = S2I + TRM2I
        ATRM = ATRM*AZ3/AD
        D1 = D1 + AK
        D2 = D2 + BK
        AD = MIN(D1,D2)
        IF (ATRM.LT.TOL*AD) GO TO 40
        AK = AK + 18.0D0
        BK = BK + 18.0D0
   30 CONTINUE
   40 CONTINUE
      IF (ID.EQ.1) GO TO 50
      AIR = S1R*C1 - C2*(ZR*S2R-ZI*S2I)
      AII = S1I*C1 - C2*(ZR*S2I+ZI*S2R)
      IF (KODE.EQ.1) RETURN
      CALL ZSQRT(ZR, ZI, STR, STI)
      ZTAR = TTH*(ZR*STR-ZI*STI)
      ZTAI = TTH*(ZR*STI+ZI*STR)
      CALL ZEXP(ZTAR, ZTAI, STR, STI)
      PTR = AIR*STR - AII*STI
      AII = AIR*STI + AII*STR
      AIR = PTR
      RETURN
   50 CONTINUE
      AIR = -S2R*C2
      AII = -S2I*C2
      IF (AZ.LE.TOL) GO TO 60
      STR = ZR*S1R - ZI*S1I
      STI = ZR*S1I + ZI*S1R
      CC = C1/(1.0D0+FID)
      AIR = AIR + CC*(STR*ZR-STI*ZI)
      AII = AII + CC*(STR*ZI+STI*ZR)
   60 CONTINUE
      IF (KODE.EQ.1) RETURN
      CALL ZSQRT(ZR, ZI, STR, STI)
      ZTAR = TTH*(ZR*STR-ZI*STI)
      ZTAI = TTH*(ZR*STI+ZI*STR)
      CALL ZEXP(ZTAR, ZTAI, STR, STI)
      PTR = STR*AIR - STI*AII
      AII = STR*AII + STI*AIR
      AIR = PTR
      RETURN
C-----------------------------------------------------------------------
C     CASE FOR ABS(Z).GT.1.0
C-----------------------------------------------------------------------
   70 CONTINUE
      FNU = (1.0D0+FID)/3.0D0
C-----------------------------------------------------------------------
C     SET PARAMETERS RELATED TO MACHINE CONSTANTS.
C     TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0D-18.
C     ELIM IS THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT.
C     EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL    AND
C     EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL       ARE INTERVALS NEAR
C     UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED ARITHMETIC IS DONE.
C     RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z.
C     DIG = NUMBER OF BASE 10 DIGITS IN TOL = 10**(-DIG).
C-----------------------------------------------------------------------
      K1 = I1MACH(15)
      K2 = I1MACH(16)
      R1M5 = D1MACH(5)
      K = MIN(ABS(K1),ABS(K2))
      ELIM = 2.303D0*(K*R1M5-3.0D0)
      K1 = I1MACH(14) - 1
      AA = R1M5*K1
      DIG = MIN(AA,18.0D0)
      AA = AA*2.303D0
      ALIM = ELIM + MAX(-AA,-41.45D0)
      RL = 1.2D0*DIG + 3.0D0
      ALAZ = LOG(AZ)
C-----------------------------------------------------------------------
C     TEST FOR PROPER RANGE
C-----------------------------------------------------------------------
      AA=0.5D0/TOL
      BB=I1MACH(9)*0.5D0
      AA=MIN(AA,BB)
      AA=AA**TTH
      IF (AZ.GT.AA) GO TO 260
      AA=SQRT(AA)
      IF (AZ.GT.AA) IERR=3
      CALL ZSQRT(ZR, ZI, CSQR, CSQI)
      ZTAR = TTH*(ZR*CSQR-ZI*CSQI)
      ZTAI = TTH*(ZR*CSQI+ZI*CSQR)
C-----------------------------------------------------------------------
C     RE(ZTA).LE.0 WHEN RE(Z).LT.0, ESPECIALLY WHEN IM(Z) IS SMALL
C-----------------------------------------------------------------------
      IFLAG = 0
      SFAC = 1.0D0
      AK = ZTAI
      IF (ZR.GE.0.0D0) GO TO 80
      BK = ZTAR
      CK = -ABS(BK)
      ZTAR = CK
      ZTAI = AK
   80 CONTINUE
      IF (ZI.NE.0.0D0) GO TO 90
      IF (ZR.GT.0.0D0) GO TO 90
      ZTAR = 0.0D0
      ZTAI = AK
   90 CONTINUE
      AA = ZTAR
      IF (AA.GE.0.0D0 .AND. ZR.GT.0.0D0) GO TO 110
      IF (KODE.EQ.2) GO TO 100
C-----------------------------------------------------------------------
C     OVERFLOW TEST
C-----------------------------------------------------------------------
      IF (AA.GT.(-ALIM)) GO TO 100
      AA = -AA + 0.25D0*ALAZ
      IFLAG = 1
      SFAC = TOL
      IF (AA.GT.ELIM) GO TO 270
  100 CONTINUE
C-----------------------------------------------------------------------
C     CBKNU AND CACON RETURN EXP(ZTA)*K(FNU,ZTA) ON KODE=2
C-----------------------------------------------------------------------
      MR = 1
      IF (ZI.LT.0.0D0) MR = -1
      CALL ZACAI(ZTAR, ZTAI, FNU, KODE, MR, 1, CYR, CYI, NN, RL, TOL,
     * ELIM, ALIM)
      IF (NN.LT.0) GO TO 280
      NZ = NZ + NN
      GO TO 130
  110 CONTINUE
      IF (KODE.EQ.2) GO TO 120
C-----------------------------------------------------------------------
C     UNDERFLOW TEST
C-----------------------------------------------------------------------
      IF (AA.LT.ALIM) GO TO 120
      AA = -AA - 0.25D0*ALAZ
      IFLAG = 2
      SFAC = 1.0D0/TOL
      IF (AA.LT.(-ELIM)) GO TO 210
  120 CONTINUE
      CALL ZBKNU(ZTAR, ZTAI, FNU, KODE, 1, CYR, CYI, NZ, TOL, ELIM,
     * ALIM)
  130 CONTINUE
      S1R = CYR(1)*COEF
      S1I = CYI(1)*COEF
      IF (IFLAG.NE.0) GO TO 150
      IF (ID.EQ.1) GO TO 140
      AIR = CSQR*S1R - CSQI*S1I
      AII = CSQR*S1I + CSQI*S1R
      RETURN
  140 CONTINUE
      AIR = -(ZR*S1R-ZI*S1I)
      AII = -(ZR*S1I+ZI*S1R)
      RETURN
  150 CONTINUE
      S1R = S1R*SFAC
      S1I = S1I*SFAC
      IF (ID.EQ.1) GO TO 160
      STR = S1R*CSQR - S1I*CSQI
      S1I = S1R*CSQI + S1I*CSQR
      S1R = STR
      AIR = S1R/SFAC
      AII = S1I/SFAC
      RETURN
  160 CONTINUE
      STR = -(S1R*ZR-S1I*ZI)
      S1I = -(S1R*ZI+S1I*ZR)
      S1R = STR
      AIR = S1R/SFAC
      AII = S1I/SFAC
      RETURN
  170 CONTINUE
      AA = 1.0D+3*D1MACH(1)
      S1R = ZEROR
      S1I = ZEROI
      IF (ID.EQ.1) GO TO 190
      IF (AZ.LE.AA) GO TO 180
      S1R = C2*ZR
      S1I = C2*ZI
  180 CONTINUE
      AIR = C1 - S1R
      AII = -S1I
      RETURN
  190 CONTINUE
      AIR = -C2
      AII = 0.0D0
      AA = SQRT(AA)
      IF (AZ.LE.AA) GO TO 200
      S1R = 0.5D0*(ZR*ZR-ZI*ZI)
      S1I = ZR*ZI
  200 CONTINUE
      AIR = AIR + C1*S1R
      AII = AII + C1*S1I
      RETURN
  210 CONTINUE
      NZ = 1
      AIR = ZEROR
      AII = ZEROI
      RETURN
  270 CONTINUE
      NZ = 0
      IERR=2
      RETURN
  280 CONTINUE
      IF(NN.EQ.(-1)) GO TO 270
      NZ=0
      IERR=5
      RETURN
  260 CONTINUE
      IERR=4
      NZ=0
      RETURN
      END
