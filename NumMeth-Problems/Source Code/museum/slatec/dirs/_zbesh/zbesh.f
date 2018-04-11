      SUBROUTINE ZBESH (ZR, ZI, FNU, KODE, M, N, CYR, CYI, NZ, IERR)
C
C     COMPLEX CY,Z,ZN,ZT,CSGN
      DOUBLE PRECISION AA, ALIM, ALN, ARG, AZ, CYI, CYR, DIG, ELIM,
     * FMM, FN, FNU, FNUL, HPI, RHPI, RL, R1M5, SGN, STR, TOL, UFL, ZI,
     * ZNI, ZNR, ZR, ZTI, D1MACH, ZABS, BB, ASCLE, RTOL, ATOL, STI,
     * CSGNR, CSGNI
      INTEGER I, IERR, INU, INUH, IR, K, KODE, K1, K2, M,
     * MM, MR, N, NN, NUF, NW, NZ, I1MACH
      DIMENSION CYR(N), CYI(N)
      EXTERNAL ZABS
C
      DATA HPI /1.57079632679489662D0/
C
C***FIRST EXECUTABLE STATEMENT  ZBESH
      IERR = 0
      NZ=0
      IF (ZR.EQ.0.0D0 .AND. ZI.EQ.0.0D0) IERR=1
      IF (FNU.LT.0.0D0) IERR=1
      IF (M.LT.1 .OR. M.GT.2) IERR=1
      IF (KODE.LT.1 .OR. KODE.GT.2) IERR=1
      IF (N.LT.1) IERR=1
      IF (IERR.NE.0) RETURN
      NN = N
C-----------------------------------------------------------------------
C     SET PARAMETERS RELATED TO MACHINE CONSTANTS.
C     TOL IS THE APPROXIMATE UNIT ROUNDOFF LIMITED TO 1.0E-18.
C     ELIM IS THE APPROXIMATE EXPONENTIAL OVER- AND UNDERFLOW LIMIT.
C     EXP(-ELIM).LT.EXP(-ALIM)=EXP(-ELIM)/TOL    AND
C     EXP(ELIM).GT.EXP(ALIM)=EXP(ELIM)*TOL       ARE INTERVALS NEAR
C     UNDERFLOW AND OVERFLOW LIMITS WHERE SCALED ARITHMETIC IS DONE.
C     RL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC EXPANSION FOR LARGE Z.
C     DIG = NUMBER OF BASE 10 DIGITS IN TOL = 10**(-DIG).
C     FNUL IS THE LOWER BOUNDARY OF THE ASYMPTOTIC SERIES FOR LARGE FNU
C-----------------------------------------------------------------------
      TOL = MAX(D1MACH(4),1.0D-18)
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
      FNUL = 10.0D0 + 6.0D0*(DIG-3.0D0)
      RL = 1.2D0*DIG + 3.0D0
      FN = FNU + (NN-1)
      MM = 3 - M - M
      FMM = MM
      ZNR = FMM*ZI
      ZNI = -FMM*ZR
C-----------------------------------------------------------------------
C     TEST FOR PROPER RANGE
C-----------------------------------------------------------------------
      AZ = ZABS(ZR,ZI)
      AA = 0.5D0/TOL
      BB = I1MACH(9)*0.5D0
      AA = MIN(AA,BB)
      IF (AZ.GT.AA) GO TO 260
      IF (FN.GT.AA) GO TO 260
      AA = SQRT(AA)
      IF (AZ.GT.AA) IERR=3
      IF (FN.GT.AA) IERR=3
C-----------------------------------------------------------------------
C     OVERFLOW TEST ON THE LAST MEMBER OF THE SEQUENCE
C-----------------------------------------------------------------------
      UFL = D1MACH(1)*1.0D+3
      IF (AZ.LT.UFL) GO TO 230
      IF (FNU.GT.FNUL) GO TO 90
      IF (FN.LE.1.0D0) GO TO 70
      IF (FN.GT.2.0D0) GO TO 60
      IF (AZ.GT.TOL) GO TO 70
      ARG = 0.5D0*AZ
      ALN = -FN*LOG(ARG)
      IF (ALN.GT.ELIM) GO TO 230
      GO TO 70
   60 CONTINUE
      CALL ZUOIK(ZNR, ZNI, FNU, KODE, 2, NN, CYR, CYI, NUF, TOL, ELIM,
     * ALIM)
      IF (NUF.LT.0) GO TO 230
      NZ = NZ + NUF
      NN = NN - NUF
C-----------------------------------------------------------------------
C     HERE NN=N OR NN=0 SINCE NUF=0,NN, OR -1 ON RETURN FROM CUOIK
C     IF NUF=NN, THEN CY(I)=CZERO FOR ALL I
C-----------------------------------------------------------------------
      IF (NN.EQ.0) GO TO 140
   70 CONTINUE
      IF ((ZNR.LT.0.0D0) .OR. (ZNR.EQ.0.0D0 .AND. ZNI.LT.0.0D0 .AND.
     * M.EQ.2)) GO TO 80
C-----------------------------------------------------------------------
C     RIGHT HALF PLANE COMPUTATION, XN.GE.0. .AND. (XN.NE.0. .OR.
C     YN.GE.0. .OR. M=1)
C-----------------------------------------------------------------------
      CALL ZBKNU(ZNR, ZNI, FNU, KODE, NN, CYR, CYI, NZ, TOL, ELIM, ALIM)
      GO TO 110
C-----------------------------------------------------------------------
C     LEFT HALF PLANE COMPUTATION
C-----------------------------------------------------------------------
   80 CONTINUE
      MR = -MM
      CALL ZACON(ZNR, ZNI, FNU, KODE, MR, NN, CYR, CYI, NW, RL, FNUL,
     * TOL, ELIM, ALIM)
      IF (NW.LT.0) GO TO 240
      NZ=NW
      GO TO 110
   90 CONTINUE
C-----------------------------------------------------------------------
C     UNIFORM ASYMPTOTIC EXPANSIONS FOR FNU.GT.FNUL
C-----------------------------------------------------------------------
      MR = 0
      IF ((ZNR.GE.0.0D0) .AND. (ZNR.NE.0.0D0 .OR. ZNI.GE.0.0D0 .OR.
     * M.NE.2)) GO TO 100
      MR = -MM
      IF (ZNR.NE.0.0D0 .OR. ZNI.GE.0.0D0) GO TO 100
      ZNR = -ZNR
      ZNI = -ZNI
  100 CONTINUE
      CALL ZBUNK(ZNR, ZNI, FNU, KODE, MR, NN, CYR, CYI, NW, TOL, ELIM,
     * ALIM)
      IF (NW.LT.0) GO TO 240
      NZ = NZ + NW
  110 CONTINUE
C-----------------------------------------------------------------------
C     H(M,FNU,Z) = -FMM*(I/HPI)*(ZT**FNU)*K(FNU,-Z*ZT)
C
C     ZT=EXP(-FMM*HPI*I) = CMPLX(0.0,-FMM), FMM=3-2*M, M=1,2
C-----------------------------------------------------------------------
      SGN = DSIGN(HPI,-FMM)
C-----------------------------------------------------------------------
C     CALCULATE EXP(FNU*HPI*I) TO MINIMIZE LOSSES OF SIGNIFICANCE
C     WHEN FNU IS LARGE
C-----------------------------------------------------------------------
      INU = FNU
      INUH = INU/2
      IR = INU - 2*INUH
      ARG = (FNU-(INU-IR))*SGN
      RHPI = 1.0D0/SGN
C     ZNI = RHPI*COS(ARG)
C     ZNR = -RHPI*SIN(ARG)
      CSGNI = RHPI*COS(ARG)
      CSGNR = -RHPI*SIN(ARG)
      IF (MOD(INUH,2).EQ.0) GO TO 120
C     ZNR = -ZNR
C     ZNI = -ZNI
      CSGNR = -CSGNR
      CSGNI = -CSGNI
  120 CONTINUE
      ZTI = -FMM
      RTOL = 1.0D0/TOL
      ASCLE = UFL*RTOL
      DO 130 I=1,NN
C       STR = CYR(I)*ZNR - CYI(I)*ZNI
C       CYI(I) = CYR(I)*ZNI + CYI(I)*ZNR
C       CYR(I) = STR
C       STR = -ZNI*ZTI
C       ZNI = ZNR*ZTI
C       ZNR = STR
        AA = CYR(I)
        BB = CYI(I)
        ATOL = 1.0D0
        IF (MAX(ABS(AA),ABS(BB)).GT.ASCLE) GO TO 135
          AA = AA*RTOL
          BB = BB*RTOL
          ATOL = TOL
  135 CONTINUE
      STR = AA*CSGNR - BB*CSGNI
      STI = AA*CSGNI + BB*CSGNR
      CYR(I) = STR*ATOL
      CYI(I) = STI*ATOL
      STR = -CSGNI*ZTI
      CSGNI = CSGNR*ZTI
      CSGNR = STR
  130 CONTINUE
      RETURN
  140 CONTINUE
      IF (ZNR.LT.0.0D0) GO TO 230
      RETURN
  230 CONTINUE
      NZ=0
      IERR=2
      RETURN
  240 CONTINUE
      IF(NW.EQ.(-1)) GO TO 230
      NZ=0
      IERR=5
      RETURN
  260 CONTINUE
      NZ=0
      IERR=4
      RETURN
      END
