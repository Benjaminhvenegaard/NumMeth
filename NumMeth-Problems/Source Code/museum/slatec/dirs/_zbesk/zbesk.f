      SUBROUTINE ZBESK (ZR, ZI, FNU, KODE, N, CYR, CYI, NZ, IERR)
C
C     COMPLEX CY,Z
      DOUBLE PRECISION AA, ALIM, ALN, ARG, AZ, CYI, CYR, DIG, ELIM, FN,
     * FNU, FNUL, RL, R1M5, TOL, UFL, ZI, ZR, D1MACH, ZABS, BB
      INTEGER IERR, K, KODE, K1, K2, MR, N, NN, NUF, NW, NZ, I1MACH
      DIMENSION CYR(N), CYI(N)
      EXTERNAL ZABS
C***FIRST EXECUTABLE STATEMENT  ZBESK
      IERR = 0
      NZ=0
      IF (ZI.EQ.0.0E0 .AND. ZR.EQ.0.0E0) IERR=1
      IF (FNU.LT.0.0D0) IERR=1
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
C-----------------------------------------------------------------------
C     TEST FOR PROPER RANGE
C-----------------------------------------------------------------------
      AZ = ZABS(ZR,ZI)
      FN = FNU + (NN-1)
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
C     UFL = EXP(-ELIM)
      UFL = D1MACH(1)*1.0D+3
      IF (AZ.LT.UFL) GO TO 180
      IF (FNU.GT.FNUL) GO TO 80
      IF (FN.LE.1.0D0) GO TO 60
      IF (FN.GT.2.0D0) GO TO 50
      IF (AZ.GT.TOL) GO TO 60
      ARG = 0.5D0*AZ
      ALN = -FN*LOG(ARG)
      IF (ALN.GT.ELIM) GO TO 180
      GO TO 60
   50 CONTINUE
      CALL ZUOIK(ZR, ZI, FNU, KODE, 2, NN, CYR, CYI, NUF, TOL, ELIM,
     * ALIM)
      IF (NUF.LT.0) GO TO 180
      NZ = NZ + NUF
      NN = NN - NUF
C-----------------------------------------------------------------------
C     HERE NN=N OR NN=0 SINCE NUF=0,NN, OR -1 ON RETURN FROM CUOIK
C     IF NUF=NN, THEN CY(I)=CZERO FOR ALL I
C-----------------------------------------------------------------------
      IF (NN.EQ.0) GO TO 100
   60 CONTINUE
      IF (ZR.LT.0.0D0) GO TO 70
C-----------------------------------------------------------------------
C     RIGHT HALF PLANE COMPUTATION, REAL(Z).GE.0.
C-----------------------------------------------------------------------
      CALL ZBKNU(ZR, ZI, FNU, KODE, NN, CYR, CYI, NW, TOL, ELIM, ALIM)
      IF (NW.LT.0) GO TO 200
      NZ=NW
      RETURN
C-----------------------------------------------------------------------
C     LEFT HALF PLANE COMPUTATION
C     PI/2.LT.ARG(Z).LE.PI AND -PI.LT.ARG(Z).LT.-PI/2.
C-----------------------------------------------------------------------
   70 CONTINUE
      IF (NZ.NE.0) GO TO 180
      MR = 1
      IF (ZI.LT.0.0D0) MR = -1
      CALL ZACON(ZR, ZI, FNU, KODE, MR, NN, CYR, CYI, NW, RL, FNUL,
     * TOL, ELIM, ALIM)
      IF (NW.LT.0) GO TO 200
      NZ=NW
      RETURN
C-----------------------------------------------------------------------
C     UNIFORM ASYMPTOTIC EXPANSIONS FOR FNU.GT.FNUL
C-----------------------------------------------------------------------
   80 CONTINUE
      MR = 0
      IF (ZR.GE.0.0D0) GO TO 90
      MR = 1
      IF (ZI.LT.0.0D0) MR = -1
   90 CONTINUE
      CALL ZBUNK(ZR, ZI, FNU, KODE, MR, NN, CYR, CYI, NW, TOL, ELIM,
     * ALIM)
      IF (NW.LT.0) GO TO 200
      NZ = NZ + NW
      RETURN
  100 CONTINUE
      IF (ZR.LT.0.0D0) GO TO 180
      RETURN
  180 CONTINUE
      NZ = 0
      IERR=2
      RETURN
  200 CONTINUE
      IF(NW.EQ.(-1)) GO TO 180
      NZ=0
      IERR=5
      RETURN
  260 CONTINUE
      NZ=0
      IERR=4
      RETURN
      END
