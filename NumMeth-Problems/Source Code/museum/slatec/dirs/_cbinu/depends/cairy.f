      SUBROUTINE CAIRY (Z, ID, KODE, AI, NZ, IERR)
      COMPLEX AI, CONE, CSQ, CY, S1, S2, TRM1, TRM2, Z, ZTA, Z3
      REAL AA, AD, AK, ALIM, ATRM, AZ, AZ3, BK, CK, COEF, C1, C2, DIG,
     * DK, D1, D2, ELIM, FID, FNU, RL, R1M5, SFAC, TOL, TTH, ZI, ZR,
     * Z3I, Z3R, R1MACH, BB, ALAZ
      INTEGER ID, IERR, IFLAG, K, KODE, K1, K2, MR, NN, NZ, I1MACH
      DIMENSION CY(1)
      DATA TTH, C1, C2, COEF /6.66666666666666667E-01,
     * 3.55028053887817240E-01,2.58819403792806799E-01,
     * 1.83776298473930683E-01/
      DATA  CONE / (1.0E0,0.0E0) /
C***FIRST EXECUTABLE STATEMENT  CAIRY
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
      IF (AZ.LT.TOL) GO TO 160
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
      AI = S1*CMPLX(C1,0.0E0) - Z*S2*CMPLX(C2,0.0E0)
      IF (KODE.EQ.1) RETURN
      ZTA = Z*CSQRT(Z)*CMPLX(TTH,0.0E0)
      AI = AI*CEXP(ZTA)
      RETURN
   50 CONTINUE
      AI = -S2*CMPLX(C2,0.0E0)
      IF (AZ.GT.TOL) AI = AI + Z*Z*S1*CMPLX(C1/(1.0E0+FID),0.0E0)
      IF (KODE.EQ.1) RETURN
      ZTA = Z*CSQRT(Z)*CMPLX(TTH,0.0E0)
      AI = AI*CEXP(ZTA)
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
      ALAZ=ALOG(AZ)
C-----------------------------------------------------------------------
C     TEST FOR RANGE
C-----------------------------------------------------------------------
      AA=0.5E0/TOL
      BB=I1MACH(9)*0.5E0
      AA=MIN(AA,BB)
      AA=AA**TTH
      IF (AZ.GT.AA) GO TO 260
      AA=SQRT(AA)
      IF (AZ.GT.AA) IERR=3
      CSQ=CSQRT(Z)
      ZTA=Z*CSQ*CMPLX(TTH,0.0E0)
C-----------------------------------------------------------------------
C     RE(ZTA).LE.0 WHEN RE(Z).LT.0, ESPECIALLY WHEN IM(Z) IS SMALL
C-----------------------------------------------------------------------
      IFLAG = 0
      SFAC = 1.0E0
      ZI = AIMAG(Z)
      ZR = REAL(Z)
      AK = AIMAG(ZTA)
      IF (ZR.GE.0.0E0) GO TO 70
      BK = REAL(ZTA)
      CK = -ABS(BK)
      ZTA = CMPLX(CK,AK)
   70 CONTINUE
      IF (ZI.NE.0.0E0) GO TO 80
      IF (ZR.GT.0.0E0) GO TO 80
      ZTA = CMPLX(0.0E0,AK)
   80 CONTINUE
      AA = REAL(ZTA)
      IF (AA.GE.0.0E0 .AND. ZR.GT.0.0E0) GO TO 100
      IF (KODE.EQ.2) GO TO 90
C-----------------------------------------------------------------------
C     OVERFLOW TEST
C-----------------------------------------------------------------------
      IF (AA.GT.(-ALIM)) GO TO 90
      AA = -AA + 0.25E0*ALAZ
      IFLAG = 1
      SFAC = TOL
      IF (AA.GT.ELIM) GO TO 240
   90 CONTINUE
C-----------------------------------------------------------------------
C     CBKNU AND CACAI RETURN EXP(ZTA)*K(FNU,ZTA) ON KODE=2
C-----------------------------------------------------------------------
      MR = 1
      IF (ZI.LT.0.0E0) MR = -1
      CALL CACAI(ZTA, FNU, KODE, MR, 1, CY, NN, RL, TOL, ELIM, ALIM)
      IF (NN.LT.0) GO TO 250
      NZ = NZ + NN
      GO TO 120
  100 CONTINUE
      IF (KODE.EQ.2) GO TO 110
C-----------------------------------------------------------------------
C     UNDERFLOW TEST
C-----------------------------------------------------------------------
      IF (AA.LT.ALIM) GO TO 110
      AA = -AA - 0.25E0*ALAZ
      IFLAG = 2
      SFAC = 1.0E0/TOL
      IF (AA.LT.(-ELIM)) GO TO 180
  110 CONTINUE
      CALL CBKNU(ZTA, FNU, KODE, 1, CY, NZ, TOL, ELIM, ALIM)
  120 CONTINUE
      S1 = CY(1)*CMPLX(COEF,0.0E0)
      IF (IFLAG.NE.0) GO TO 140
      IF (ID.EQ.1) GO TO 130
      AI = CSQ*S1
      RETURN
  130 AI = -Z*S1
      RETURN
  140 CONTINUE
      S1 = S1*CMPLX(SFAC,0.0E0)
      IF (ID.EQ.1) GO TO 150
      S1 = S1*CSQ
      AI = S1*CMPLX(1.0E0/SFAC,0.0E0)
      RETURN
  150 CONTINUE
      S1 = -S1*Z
      AI = S1*CMPLX(1.0E0/SFAC,0.0E0)
      RETURN
  160 CONTINUE
      AA = 1.0E+3*R1MACH(1)
      S1 = CMPLX(0.0E0,0.0E0)
      IF (ID.EQ.1) GO TO 170
      IF (AZ.GT.AA) S1 = CMPLX(C2,0.0E0)*Z
      AI = CMPLX(C1,0.0E0) - S1
      RETURN
  170 CONTINUE
      AI = -CMPLX(C2,0.0E0)
      AA = SQRT(AA)
      IF (AZ.GT.AA) S1 = Z*Z*CMPLX(0.5E0,0.0E0)
      AI = AI + S1*CMPLX(C1,0.0E0)
      RETURN
  180 CONTINUE
      NZ = 1
      AI = CMPLX(0.0E0,0.0E0)
      RETURN
  240 CONTINUE
      NZ = 0
      IERR=2
      RETURN
  250 CONTINUE
      IF(NN.EQ.(-1)) GO TO 240
      NZ=0
      IERR=5
      RETURN
  260 CONTINUE
      IERR=4
      NZ=0
      RETURN
      END
