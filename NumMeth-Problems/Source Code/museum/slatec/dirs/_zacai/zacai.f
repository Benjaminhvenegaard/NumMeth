      SUBROUTINE ZACAI (ZR, ZI, FNU, KODE, MR, N, YR, YI, NZ, RL, TOL,
     +   ELIM, ALIM)
C     COMPLEX CSGN,CSPN,C1,C2,Y,Z,ZN,CY
      DOUBLE PRECISION ALIM, ARG, ASCLE, AZ, CSGNR, CSGNI, CSPNR,
     * CSPNI, C1R, C1I, C2R, C2I, CYR, CYI, DFNU, ELIM, FMR, FNU, PI,
     * RL, SGN, TOL, YY, YR, YI, ZR, ZI, ZNR, ZNI, D1MACH, ZABS
      INTEGER INU, IUF, KODE, MR, N, NN, NW, NZ
      DIMENSION YR(N), YI(N), CYR(2), CYI(2)
      EXTERNAL ZABS
      DATA PI / 3.14159265358979324D0 /
C***FIRST EXECUTABLE STATEMENT  ZACAI
      NZ = 0
      ZNR = -ZR
      ZNI = -ZI
      AZ = ZABS(ZR,ZI)
      NN = N
      DFNU = FNU + (N-1)
      IF (AZ.LE.2.0D0) GO TO 10
      IF (AZ*AZ*0.25D0.GT.DFNU+1.0D0) GO TO 20
   10 CONTINUE
C-----------------------------------------------------------------------
C     POWER SERIES FOR THE I FUNCTION
C-----------------------------------------------------------------------
      CALL ZSERI(ZNR, ZNI, FNU, KODE, NN, YR, YI, NW, TOL, ELIM, ALIM)
      GO TO 40
   20 CONTINUE
      IF (AZ.LT.RL) GO TO 30
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR LARGE Z FOR THE I FUNCTION
C-----------------------------------------------------------------------
      CALL ZASYI(ZNR, ZNI, FNU, KODE, NN, YR, YI, NW, RL, TOL, ELIM,
     * ALIM)
      IF (NW.LT.0) GO TO 80
      GO TO 40
   30 CONTINUE
C-----------------------------------------------------------------------
C     MILLER ALGORITHM NORMALIZED BY THE SERIES FOR THE I FUNCTION
C-----------------------------------------------------------------------
      CALL ZMLRI(ZNR, ZNI, FNU, KODE, NN, YR, YI, NW, TOL)
      IF(NW.LT.0) GO TO 80
   40 CONTINUE
C-----------------------------------------------------------------------
C     ANALYTIC CONTINUATION TO THE LEFT HALF PLANE FOR THE K FUNCTION
C-----------------------------------------------------------------------
      CALL ZBKNU(ZNR, ZNI, FNU, KODE, 1, CYR, CYI, NW, TOL, ELIM, ALIM)
      IF (NW.NE.0) GO TO 80
      FMR = MR
      SGN = -DSIGN(PI,FMR)
      CSGNR = 0.0D0
      CSGNI = SGN
      IF (KODE.EQ.1) GO TO 50
      YY = -ZNI
      CSGNR = -CSGNI*SIN(YY)
      CSGNI = CSGNI*COS(YY)
   50 CONTINUE
C-----------------------------------------------------------------------
C     CALCULATE CSPN=EXP(FNU*PI*I) TO MINIMIZE LOSSES OF SIGNIFICANCE
C     WHEN FNU IS LARGE
C-----------------------------------------------------------------------
      INU = FNU
      ARG = (FNU-INU)*SGN
      CSPNR = COS(ARG)
      CSPNI = SIN(ARG)
      IF (MOD(INU,2).EQ.0) GO TO 60
      CSPNR = -CSPNR
      CSPNI = -CSPNI
   60 CONTINUE
      C1R = CYR(1)
      C1I = CYI(1)
      C2R = YR(1)
      C2I = YI(1)
      IF (KODE.EQ.1) GO TO 70
      IUF = 0
      ASCLE = 1.0D+3*D1MACH(1)/TOL
      CALL ZS1S2(ZNR, ZNI, C1R, C1I, C2R, C2I, NW, ASCLE, ALIM, IUF)
      NZ = NZ + NW
   70 CONTINUE
      YR(1) = CSPNR*C1R - CSPNI*C1I + CSGNR*C2R - CSGNI*C2I
      YI(1) = CSPNR*C1I + CSPNI*C1R + CSGNR*C2I + CSGNI*C2R
      RETURN
   80 CONTINUE
      NZ = -1
      IF(NW.EQ.(-2)) NZ=-2
      RETURN
      END
