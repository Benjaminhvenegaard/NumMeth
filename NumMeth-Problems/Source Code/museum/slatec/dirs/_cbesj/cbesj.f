      SUBROUTINE CBESJ (Z, FNU, KODE, N, CY, NZ, IERR)
C
      COMPLEX CI, CSGN, CY, Z, ZN
      REAL AA, ALIM, ARG, DIG, ELIM, FNU, FNUL, HPI, RL, R1, R1M5, R2,
     * TOL, YY, R1MACH, AZ, FN, BB, ASCLE, RTOL, ATOL
      INTEGER I, IERR, INU, INUH, IR, KODE, K1, K2, N, NL, NZ, I1MACH, K
      DIMENSION CY(N)
      DATA HPI /1.57079632679489662E0/
C
C***FIRST EXECUTABLE STATEMENT  CBESJ
      IERR = 0
      NZ=0
      IF (FNU.LT.0.0E0) IERR=1
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
      CI = CMPLX(0.0E0,1.0E0)
      YY = AIMAG(Z)
      AZ = ABS(Z)
C-----------------------------------------------------------------------
C     TEST FOR RANGE
C-----------------------------------------------------------------------
      AA = 0.5E0/TOL
      BB=I1MACH(9)*0.5E0
      AA=MIN(AA,BB)
      FN=FNU+(N-1)
      IF(AZ.GT.AA) GO TO 140
      IF(FN.GT.AA) GO TO 140
      AA=SQRT(AA)
      IF(AZ.GT.AA) IERR=3
      IF(FN.GT.AA) IERR=3
C-----------------------------------------------------------------------
C     CALCULATE CSGN=EXP(FNU*HPI*I) TO MINIMIZE LOSSES OF SIGNIFICANCE
C     WHEN FNU IS LARGE
C-----------------------------------------------------------------------
      INU = FNU
      INUH = INU/2
      IR = INU - 2*INUH
      ARG = (FNU-(INU-IR))*HPI
      R1 = COS(ARG)
      R2 = SIN(ARG)
      CSGN = CMPLX(R1,R2)
      IF (MOD(INUH,2).EQ.1) CSGN = -CSGN
C-----------------------------------------------------------------------
C     ZN IS IN THE RIGHT HALF PLANE
C-----------------------------------------------------------------------
      ZN = -Z*CI
      IF (YY.GE.0.0E0) GO TO 40
      ZN = -ZN
      CSGN = CONJG(CSGN)
      CI = CONJG(CI)
   40 CONTINUE
      CALL CBINU(ZN, FNU, KODE, N, CY, NZ, RL, FNUL, TOL, ELIM, ALIM)
      IF (NZ.LT.0) GO TO 120
      NL = N - NZ
      IF (NL.EQ.0) RETURN
      RTOL = 1.0E0/TOL
      ASCLE = R1MACH(1)*RTOL*1.0E+3
      DO 50 I=1,NL
C       CY(I)=CY(I)*CSGN
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
        CSGN = CSGN*CI
   50 CONTINUE
      RETURN
  120 CONTINUE
      IF(NZ.EQ.(-2)) GO TO 130
      NZ = 0
      IERR = 2
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
