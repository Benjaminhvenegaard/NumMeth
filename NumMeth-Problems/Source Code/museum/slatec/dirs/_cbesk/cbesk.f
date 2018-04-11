      SUBROUTINE CBESK (Z, FNU, KODE, N, CY, NZ, IERR)
C
      COMPLEX CY, Z
      REAL AA, ALIM, ALN, ARG, AZ, DIG, ELIM, FN, FNU, FNUL, RL, R1M5,
     * TOL, UFL, XX, YY, R1MACH, BB
      INTEGER IERR, K, KODE, K1, K2, MR, N, NN, NUF, NW, NZ, I1MACH
      DIMENSION CY(N)
C***FIRST EXECUTABLE STATEMENT  CBESK
      IERR = 0
      NZ=0
      XX = REAL(Z)
      YY = AIMAG(Z)
      IF (YY.EQ.0.0E0 .AND. XX.EQ.0.0E0) IERR=1
      IF (FNU.LT.0.0E0) IERR=1
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
      TOL = MAX(R1MACH(4),1.0E-18)
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
      FNUL = 10.0E0 + 6.0E0*(DIG-3.0E0)
      RL = 1.2E0*DIG + 3.0E0
      AZ = ABS(Z)
      FN = FNU + (NN-1)
C-----------------------------------------------------------------------
C     TEST FOR RANGE
C-----------------------------------------------------------------------
      AA = 0.5E0/TOL
      BB=I1MACH(9)*0.5E0
      AA=MIN(AA,BB)
      IF(AZ.GT.AA) GO TO 210
      IF(FN.GT.AA) GO TO 210
      AA=SQRT(AA)
      IF(AZ.GT.AA) IERR=3
      IF(FN.GT.AA) IERR=3
C-----------------------------------------------------------------------
C     OVERFLOW TEST ON THE LAST MEMBER OF THE SEQUENCE
C-----------------------------------------------------------------------
C     UFL = EXP(-ELIM)
      UFL = R1MACH(1)*1.0E+3
      IF (AZ.LT.UFL) GO TO 180
      IF (FNU.GT.FNUL) GO TO 80
      IF (FN.LE.1.0E0) GO TO 60
      IF (FN.GT.2.0E0) GO TO 50
      IF (AZ.GT.TOL) GO TO 60
      ARG = 0.5E0*AZ
      ALN = -FN*ALOG(ARG)
      IF (ALN.GT.ELIM) GO TO 180
      GO TO 60
   50 CONTINUE
      CALL CUOIK(Z, FNU, KODE, 2, NN, CY, NUF, TOL, ELIM, ALIM)
      IF (NUF.LT.0) GO TO 180
      NZ = NZ + NUF
      NN = NN - NUF
C-----------------------------------------------------------------------
C     HERE NN=N OR NN=0 SINCE NUF=0,NN, OR -1 ON RETURN FROM CUOIK
C     IF NUF=NN, THEN CY(I)=CZERO FOR ALL I
C-----------------------------------------------------------------------
      IF (NN.EQ.0) GO TO 100
   60 CONTINUE
      IF (XX.LT.0.0E0) GO TO 70
C-----------------------------------------------------------------------
C     RIGHT HALF PLANE COMPUTATION, REAL(Z).GE.0.
C-----------------------------------------------------------------------
      CALL CBKNU(Z, FNU, KODE, NN, CY, NW, TOL, ELIM, ALIM)
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
      IF (YY.LT.0.0E0) MR = -1
      CALL CACON(Z, FNU, KODE, MR, NN, CY, NW, RL, FNUL, TOL, ELIM,
     * ALIM)
      IF (NW.LT.0) GO TO 200
      NZ=NW
      RETURN
C-----------------------------------------------------------------------
C     UNIFORM ASYMPTOTIC EXPANSIONS FOR FNU.GT.FNUL
C-----------------------------------------------------------------------
   80 CONTINUE
      MR = 0
      IF (XX.GE.0.0E0) GO TO 90
      MR = 1
      IF (YY.LT.0.0E0) MR = -1
   90 CONTINUE
      CALL CBUNK(Z, FNU, KODE, MR, NN, CY, NW, TOL, ELIM, ALIM)
      IF (NW.LT.0) GO TO 200
      NZ = NZ + NW
      RETURN
  100 CONTINUE
      IF (XX.LT.0.0E0) GO TO 180
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
  210 CONTINUE
      NZ=0
      IERR=4
      RETURN
      END
