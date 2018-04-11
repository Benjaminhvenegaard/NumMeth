      SUBROUTINE ZBESI (ZR, ZI, FNU, KODE, N, CYR, CYI, NZ, IERR)
C     COMPLEX CONE,CSGN,CW,CY,CZERO,Z,ZN
      DOUBLE PRECISION AA, ALIM, ARG, CONEI, CONER, CSGNI, CSGNR, CYI,
     * CYR, DIG, ELIM, FNU, FNUL, PI, RL, R1M5, STR, TOL, ZI, ZNI, ZNR,
     * ZR, D1MACH, AZ, BB, FN, ZABS, ASCLE, RTOL, ATOL, STI
      INTEGER I, IERR, INU, K, KODE, K1,K2,N,NZ,NN, I1MACH
      DIMENSION CYR(N), CYI(N)
      EXTERNAL ZABS
      DATA PI /3.14159265358979324D0/
      DATA CONER, CONEI /1.0D0,0.0D0/
C
C***FIRST EXECUTABLE STATEMENT  ZBESI
      IERR = 0
      NZ=0
      IF (FNU.LT.0.0D0) IERR=1
      IF (KODE.LT.1 .OR. KODE.GT.2) IERR=1
      IF (N.LT.1) IERR=1
      IF (IERR.NE.0) RETURN
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
      RL = 1.2D0*DIG + 3.0D0
      FNUL = 10.0D0 + 6.0D0*(DIG-3.0D0)
C-----------------------------------------------------------------------
C     TEST FOR PROPER RANGE
C-----------------------------------------------------------------------
      AZ = ZABS(ZR,ZI)
      FN = FNU+(N-1)
      AA = 0.5D0/TOL
      BB=I1MACH(9)*0.5D0
      AA = MIN(AA,BB)
      IF (AZ.GT.AA) GO TO 260
      IF (FN.GT.AA) GO TO 260
      AA = SQRT(AA)
      IF (AZ.GT.AA) IERR=3
      IF (FN.GT.AA) IERR=3
      ZNR = ZR
      ZNI = ZI
      CSGNR = CONER
      CSGNI = CONEI
      IF (ZR.GE.0.0D0) GO TO 40
      ZNR = -ZR
      ZNI = -ZI
C-----------------------------------------------------------------------
C     CALCULATE CSGN=EXP(FNU*PI*I) TO MINIMIZE LOSSES OF SIGNIFICANCE
C     WHEN FNU IS LARGE
C-----------------------------------------------------------------------
      INU = FNU
      ARG = (FNU-INU)*PI
      IF (ZI.LT.0.0D0) ARG = -ARG
      CSGNR = COS(ARG)
      CSGNI = SIN(ARG)
      IF (MOD(INU,2).EQ.0) GO TO 40
      CSGNR = -CSGNR
      CSGNI = -CSGNI
   40 CONTINUE
      CALL ZBINU(ZNR, ZNI, FNU, KODE, N, CYR, CYI, NZ, RL, FNUL, TOL,
     * ELIM, ALIM)
      IF (NZ.LT.0) GO TO 120
      IF (ZR.GE.0.0D0) RETURN
C-----------------------------------------------------------------------
C     ANALYTIC CONTINUATION TO THE LEFT HALF PLANE
C-----------------------------------------------------------------------
      NN = N - NZ
      IF (NN.EQ.0) RETURN
      RTOL = 1.0D0/TOL
      ASCLE = D1MACH(1)*RTOL*1.0D+3
      DO 50 I=1,NN
C       STR = CYR(I)*CSGNR - CYI(I)*CSGNI
C       CYI(I) = CYR(I)*CSGNI + CYI(I)*CSGNR
C       CYR(I) = STR
        AA = CYR(I)
        BB = CYI(I)
        ATOL = 1.0D0
        IF (MAX(ABS(AA),ABS(BB)).GT.ASCLE) GO TO 55
          AA = AA*RTOL
          BB = BB*RTOL
          ATOL = TOL
   55   CONTINUE
        STR = AA*CSGNR - BB*CSGNI
        STI = AA*CSGNI + BB*CSGNR
        CYR(I) = STR*ATOL
        CYI(I) = STI*ATOL
        CSGNR = -CSGNR
        CSGNI = -CSGNI
   50 CONTINUE
      RETURN
  120 CONTINUE
      IF(NZ.EQ.(-2)) GO TO 130
      NZ = 0
      IERR=2
      RETURN
  130 CONTINUE
      NZ=0
      IERR=5
      RETURN
  260 CONTINUE
      NZ=0
      IERR=4
      RETURN
      END
