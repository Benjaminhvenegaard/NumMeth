      SUBROUTINE CBESI (Z, FNU, KODE, N, CY, NZ, IERR)
      COMPLEX CONE, CSGN, CY, Z, ZN
      REAL AA, ALIM, ARG, DIG, ELIM, FNU, FNUL, PI, RL, R1M5, S1, S2,
     * TOL, XX, YY, R1MACH, AZ, FN, BB, ASCLE, RTOL, ATOL
      INTEGER I, IERR, INU, K, KODE, K1, K2, N, NN, NZ, I1MACH
      DIMENSION CY(N)
      DATA PI /3.14159265358979324E0/
      DATA CONE / (1.0E0,0.0E0) /
C
C***FIRST EXECUTABLE STATEMENT  CBESI
      IERR = 0
      NZ=0
      IF (FNU.LT.0.0E0) IERR=1
      IF (KODE.LT.1 .OR. KODE.GT.2) IERR=1
      IF (N.LT.1) IERR=1
      IF (IERR.NE.0) RETURN
      XX = REAL(Z)
      YY = AIMAG(Z)
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
      RL = 1.2E0*DIG + 3.0E0
      FNUL = 10.0E0 + 6.0E0*(DIG-3.0E0)
      AZ = ABS(Z)
C-----------------------------------------------------------------------
C     TEST FOR RANGE
C-----------------------------------------------------------------------
      AA = 0.5E0/TOL
      BB=I1MACH(9)*0.5E0
      AA=MIN(AA,BB)
      IF(AZ.GT.AA) GO TO 140
      FN=FNU+(N-1)
      IF(FN.GT.AA) GO TO 140
      AA=SQRT(AA)
      IF(AZ.GT.AA) IERR=3
      IF(FN.GT.AA) IERR=3
      ZN = Z
      CSGN = CONE
      IF (XX.GE.0.0E0) GO TO 40
      ZN = -Z
C-----------------------------------------------------------------------
C     CALCULATE CSGN=EXP(FNU*PI*I) TO MINIMIZE LOSSES OF SIGNIFICANCE
C     WHEN FNU IS LARGE
C-----------------------------------------------------------------------
      INU = FNU
      ARG = (FNU-INU)*PI
      IF (YY.LT.0.0E0) ARG = -ARG
      S1 = COS(ARG)
      S2 = SIN(ARG)
      CSGN = CMPLX(S1,S2)
      IF (MOD(INU,2).EQ.1) CSGN = -CSGN
   40 CONTINUE
      CALL CBINU(ZN, FNU, KODE, N, CY, NZ, RL, FNUL, TOL, ELIM, ALIM)
      IF (NZ.LT.0) GO TO 120
      IF (XX.GE.0.0E0) RETURN
C-----------------------------------------------------------------------
C     ANALYTIC CONTINUATION TO THE LEFT HALF PLANE
C-----------------------------------------------------------------------
      NN = N - NZ
      IF (NN.EQ.0) RETURN
      RTOL = 1.0E0/TOL
      ASCLE = R1MACH(1)*RTOL*1.0E+3
      DO 50 I=1,NN
C       CY(I) = CY(I)*CSGN
        ZN=CY(I)
        AA=REAL(ZN)
        BB=AIMAG(ZN)
        ATOL=1.0E0
        IF (MAX(ABS(AA),ABS(BB)).GT.ASCLE) GO TO 55
          ZN = ZN*CMPLX(RTOL,0.0E0)
          ATOL = TOL
   55   CONTINUE
        ZN = ZN*CSGN
        CY(I) = ZN*CMPLX(ATOL,0.0E0)
        CSGN = -CSGN
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
  140 CONTINUE
      NZ=0
      IERR=4
      RETURN
      END
