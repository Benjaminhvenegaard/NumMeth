      SUBROUTINE CBIRY (Z, ID, KODE, BI, IERR)
      COMPLEX BI, CONE, CSQ, CY, S1, S2, TRM1, TRM2, Z, ZTA, Z3
      REAL AA, AD, AK, ALIM, ATRM, AZ, AZ3, BB, BK, CK, COEF, C1, C2,
     * DIG, DK, D1, D2, ELIM, FID, FMR, FNU, FNUL, PI, RL, R1M5, SFAC,
     * TOL, TTH, ZI, ZR, Z3I, Z3R, R1MACH
      INTEGER ID, IERR, K, KODE, K1, K2, NZ, I1MACH
      DIMENSION CY(2)
      DATA TTH, C1, C2, COEF, PI /6.66666666666666667E-01,
     * 6.14926627446000736E-01,4.48288357353826359E-01,
     * 5.77350269189625765E-01,3.14159265358979324E+00/
      DATA  CONE / (1.0E0,0.0E0) /
C***FIRST EXECUTABLE STATEMENT  CBIRY
      IERR = 0
      NZ=0
      IF (ID.LT.0 .OR. ID.GT.1) IERR=1
      IF (KODE.LT.1 .OR. KODE.GT.2) IERR=1
      IF (IERR.NE.0) RETURN
      AZ = ABS(Z)
      TOL = MAX(R1MACH(4),1.0E-18)
      FID = ID
      IF (AZ.GT.1.0E0) GO TO 60
C-----------------------------------------------------------------------
C     POWER SERIES FOR ABS(Z).LE.1.
C-----------------------------------------------------------------------
      S1 = CONE
      S2 = CONE
      IF (AZ.LT.TOL) GO TO 110
      AA = AZ*AZ
      IF (AA.LT.TOL/AZ) GO TO 40
      TRM1 = CONE
      TRM2 = CONE
      ATRM = 1.0E0
      Z3 = Z*Z*Z
      AZ3 = AZ*AA
      AK = 2.0E0 + FID
      BK = 3.0E0 - FID - FID
      CK = 4.0E0 - FID
      DK = 3.0E0 + FID + FID
      D1 = AK*DK
      D2 = BK*CK
      AD = MIN(D1,D2)
      AK = 24.0E0 + 9.0E0*FID
      BK = 30.0E0 - 9.0E0*FID
      Z3R = REAL(Z3)
      Z3I = AIMAG(Z3)
      DO 30 K=1,25
        TRM1 = TRM1*CMPLX(Z3R/D1,Z3I/D1)
        S1 = S1 + TRM1
        TRM2 = TRM2*CMPLX(Z3R/D2,Z3I/D2)
        S2 = S2 + TRM2
        ATRM = ATRM*AZ3/AD
        D1 = D1 + AK
        D2 = D2 + BK
        AD = MIN(D1,D2)
        IF (ATRM.LT.TOL*AD) GO TO 40
        AK = AK + 18.0E0
        BK = BK + 18.0E0
   30 CONTINUE
   40 CONTINUE
      IF (ID.EQ.1) GO TO 50
      BI = S1*CMPLX(C1,0.0E0) + Z*S2*CMPLX(C2,0.0E0)
      IF (KODE.EQ.1) RETURN
      ZTA = Z*CSQRT(Z)*CMPLX(TTH,0.0E0)
      AA = REAL(ZTA)
      AA = -ABS(AA)
      BI = BI*CMPLX(EXP(AA),0.0E0)
      RETURN
   50 CONTINUE
      BI = S2*CMPLX(C2,0.0E0)
      IF (AZ.GT.TOL) BI = BI + Z*Z*S1*CMPLX(C1/(1.0E0+FID),0.0E0)
      IF (KODE.EQ.1) RETURN
      ZTA = Z*CSQRT(Z)*CMPLX(TTH,0.0E0)
      AA = REAL(ZTA)
      AA = -ABS(AA)
      BI = BI*CMPLX(EXP(AA),0.0E0)
      RETURN
C-----------------------------------------------------------------------
C     CASE FOR ABS(Z).GT.1.0
C-----------------------------------------------------------------------
   60 CONTINUE
      FNU = (1.0E0+FID)/3.0E0
C-----------------------------------------------------------------------
C     SET PARAMETERS RELATED TO MACHINE CONSTANTS.
C     TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18.
C     ELIM IS THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT.
C     EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL    AND
C     EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL       ARE INTERVALS NEAR
C     UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED ARITHMETIC IS DONE.
C     RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z.
C     DIG = NUMBER OF BASE 10 DIGITS IN TOL = 10**(-DIG).
C     FNUL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC SERIES FOR LARGE FNU.
C-----------------------------------------------------------------------
      K1 = I1MACH(12)
      K2 = I1MACH(13)
      R1M5 = R1MACH(5)
      K = MIN(ABS(K1),ABS(K2))
      ELIM = 2.303E0*(K*R1M5-3.0E0)
      K1 = I1MACH(11) - 1
      AA = R1M5*K1
      DIG = MIN(AA,18.0E0)
      AA = AA*2.303E0
      ALIM = ELIM + MAX(-AA,-41.45E0)
      RL = 1.2E0*DIG + 3.0E0
      FNUL = 10.0E0 + 6.0E0*(DIG-3.0E0)
C-----------------------------------------------------------------------
C     TEST FOR RANGE
C-----------------------------------------------------------------------
      AA=0.5E0/TOL
      BB=I1MACH(9)*0.5E0
      AA=MIN(AA,BB)
      AA=AA**TTH
      IF (AZ.GT.AA) GO TO 190
      AA=SQRT(AA)
      IF (AZ.GT.AA) IERR=3
      CSQ=CSQRT(Z)
      ZTA=Z*CSQ*CMPLX(TTH,0.0E0)
C-----------------------------------------------------------------------
C     RE(ZTA).LE.0 WHEN RE(Z).LT.0, ESPECIALLY WHEN IM(Z) IS SMALL
C-----------------------------------------------------------------------
      SFAC = 1.0E0
      ZI = AIMAG(Z)
      ZR = REAL(Z)
      AK = AIMAG(ZTA)
      IF (ZR.GE.0.0E0) GO TO 70
      BK = REAL(ZTA)
      CK = -ABS(BK)
      ZTA = CMPLX(CK,AK)
   70 CONTINUE
      IF (ZI.EQ.0.0E0 .AND. ZR.LE.0.0E0) ZTA = CMPLX(0.0E0,AK)
      AA = REAL(ZTA)
      IF (KODE.EQ.2) GO TO 80
C-----------------------------------------------------------------------
C     OVERFLOW TEST
C-----------------------------------------------------------------------
      BB = ABS(AA)
      IF (BB.LT.ALIM) GO TO 80
      BB = BB + 0.25E0*ALOG(AZ)
      SFAC = TOL
      IF (BB.GT.ELIM) GO TO 170
   80 CONTINUE
      FMR = 0.0E0
      IF (AA.GE.0.0E0 .AND. ZR.GT.0.0E0) GO TO 90
      FMR = PI
      IF (ZI.LT.0.0E0) FMR = -PI
      ZTA = -ZTA
   90 CONTINUE
C-----------------------------------------------------------------------
C     AA=FACTOR FOR ANALYTIC CONTINUATION OF I(FNU,ZTA)
C     KODE=2 RETURNS EXP(-ABS(XZTA))*I(FNU,ZTA) FROM CBINU
C-----------------------------------------------------------------------
      CALL CBINU(ZTA, FNU, KODE, 1, CY, NZ, RL, FNUL, TOL, ELIM, ALIM)
      IF (NZ.LT.0) GO TO 180
      AA = FMR*FNU
      Z3 = CMPLX(SFAC,0.0E0)
      S1 = CY(1)*CMPLX(COS(AA),SIN(AA))*Z3
      FNU = (2.0E0-FID)/3.0E0
      CALL CBINU(ZTA, FNU, KODE, 2, CY, NZ, RL, FNUL, TOL, ELIM, ALIM)
      CY(1) = CY(1)*Z3
      CY(2) = CY(2)*Z3
C-----------------------------------------------------------------------
C     BACKWARD RECUR ONE STEP FOR ORDERS -1/3 OR -2/3
C-----------------------------------------------------------------------
      S2 = CY(1)*CMPLX(FNU+FNU,0.0E0)/ZTA + CY(2)
      AA = FMR*(FNU-1.0E0)
      S1 = (S1+S2*CMPLX(COS(AA),SIN(AA)))*CMPLX(COEF,0.0E0)
      IF (ID.EQ.1) GO TO 100
      S1 = CSQ*S1
      BI = S1*CMPLX(1.0E0/SFAC,0.0E0)
      RETURN
  100 CONTINUE
      S1 = Z*S1
      BI = S1*CMPLX(1.0E0/SFAC,0.0E0)
      RETURN
  110 CONTINUE
      AA = C1*(1.0E0-FID) + FID*C2
      BI = CMPLX(AA,0.0E0)
      RETURN
  170 CONTINUE
      NZ=0
      IERR=2
      RETURN
  180 CONTINUE
      IF(NZ.EQ.(-1)) GO TO 170
      NZ=0
      IERR=5
      RETURN
  190 CONTINUE
      IERR=4
      NZ=0
      RETURN
      END
