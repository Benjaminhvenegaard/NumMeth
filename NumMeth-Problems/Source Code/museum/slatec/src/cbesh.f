      SUBROUTINE CBESH (Z, FNU, KODE, M, N, CY, NZ, IERR)
C
      COMPLEX CY, Z, ZN, ZT, CSGN
      REAL AA, ALIM, ALN, ARG, AZ, CPN, DIG, ELIM, FMM, FN, FNU, FNUL,
     * HPI, RHPI, RL, R1M5, SGN, SPN, TOL, UFL, XN, XX, YN, YY, R1MACH,
     * BB, ASCLE, RTOL, ATOL
      INTEGER I, IERR, INU, INUH, IR, K, KODE, K1, K2, M,
     * MM, MR, N, NN, NUF, NW, NZ, I1MACH
      DIMENSION CY(N)
C
      DATA HPI /1.57079632679489662E0/
C
C***FIRST EXECUTABLE STATEMENT  CBESH
      NZ=0
      XX = REAL(Z)
      YY = AIMAG(Z)
      IERR = 0
      IF (XX.EQ.0.0E0 .AND. YY.EQ.0.0E0) IERR=1
      IF (FNU.LT.0.0E0) IERR=1
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
      FN = FNU + (NN-1)
      MM = 3 - M - M
      FMM = MM
      ZN = Z*CMPLX(0.0E0,-FMM)
      XN = REAL(ZN)
      YN = AIMAG(ZN)
      AZ = ABS(Z)
C-----------------------------------------------------------------------
C     TEST FOR RANGE
C-----------------------------------------------------------------------
      AA = 0.5E0/TOL
      BB=I1MACH(9)*0.5E0
      AA=MIN(AA,BB)
      IF(AZ.GT.AA) GO TO 240
      IF(FN.GT.AA) GO TO 240
      AA=SQRT(AA)
      IF(AZ.GT.AA) IERR=3
      IF(FN.GT.AA) IERR=3
C-----------------------------------------------------------------------
C     OVERFLOW TEST ON THE LAST MEMBER OF THE SEQUENCE
C-----------------------------------------------------------------------
      UFL = R1MACH(1)*1.0E+3
      IF (AZ.LT.UFL) GO TO 220
      IF (FNU.GT.FNUL) GO TO 90
      IF (FN.LE.1.0E0) GO TO 70
      IF (FN.GT.2.0E0) GO TO 60
      IF (AZ.GT.TOL) GO TO 70
      ARG = 0.5E0*AZ
      ALN = -FN*ALOG(ARG)
      IF (ALN.GT.ELIM) GO TO 220
      GO TO 70
   60 CONTINUE
      CALL CUOIK(ZN, FNU, KODE, 2, NN, CY, NUF, TOL, ELIM, ALIM)
      IF (NUF.LT.0) GO TO 220
      NZ = NZ + NUF
      NN = NN - NUF
C-----------------------------------------------------------------------
C     HERE NN=N OR NN=0 SINCE NUF=0,NN, OR -1 ON RETURN FROM CUOIK
C     IF NUF=NN, THEN CY(I)=CZERO FOR ALL I
C-----------------------------------------------------------------------
      IF (NN.EQ.0) GO TO 130
   70 CONTINUE
      IF ((XN.LT.0.0E0) .OR. (XN.EQ.0.0E0 .AND. YN.LT.0.0E0 .AND.
     * M.EQ.2)) GO TO 80
C-----------------------------------------------------------------------
C     RIGHT HALF PLANE COMPUTATION, XN.GE.0. .AND. (XN.NE.0. .OR.
C     YN.GE.0. .OR. M=1)
C-----------------------------------------------------------------------
      CALL CBKNU(ZN, FNU, KODE, NN, CY, NZ, TOL, ELIM, ALIM)
      GO TO 110
C-----------------------------------------------------------------------
C     LEFT HALF PLANE COMPUTATION
C-----------------------------------------------------------------------
   80 CONTINUE
      MR = -MM
      CALL CACON(ZN, FNU, KODE, MR, NN, CY, NW, RL, FNUL, TOL, ELIM,
     * ALIM)
      IF (NW.LT.0) GO TO 230
      NZ=NW
      GO TO 110
   90 CONTINUE
C-----------------------------------------------------------------------
C     UNIFORM ASYMPTOTIC EXPANSIONS FOR FNU.GT.FNUL
C-----------------------------------------------------------------------
      MR = 0
      IF ((XN.GE.0.0E0) .AND. (XN.NE.0.0E0 .OR. YN.GE.0.0E0 .OR.
     * M.NE.2)) GO TO 100
      MR = -MM
      IF (XN.EQ.0.0E0 .AND. YN.LT.0.0E0) ZN = -ZN
  100 CONTINUE
      CALL CBUNK(ZN, FNU, KODE, MR, NN, CY, NW, TOL, ELIM, ALIM)
      IF (NW.LT.0) GO TO 230
      NZ = NZ + NW
  110 CONTINUE
C-----------------------------------------------------------------------
C     H(M,FNU,Z) = -FMM*(I/HPI)*(ZT**FNU)*K(FNU,-Z*ZT)
C
C     ZT=EXP(-FMM*HPI*I) = CMPLX(0.0,-FMM), FMM=3-2*M, M=1,2
C-----------------------------------------------------------------------
      SGN = SIGN(HPI,-FMM)
C-----------------------------------------------------------------------
C     CALCULATE EXP(FNU*HPI*I) TO MINIMIZE LOSSES OF SIGNIFICANCE
C     WHEN FNU IS LARGE
C-----------------------------------------------------------------------
      INU = FNU
      INUH = INU/2
      IR = INU - 2*INUH
      ARG = (FNU-(INU-IR))*SGN
      RHPI = 1.0E0/SGN
      CPN = RHPI*COS(ARG)
      SPN = RHPI*SIN(ARG)
C     ZN = CMPLX(-SPN,CPN)
      CSGN = CMPLX(-SPN,CPN)
C     IF (MOD(INUH,2).EQ.1) ZN = -ZN
      IF (MOD(INUH,2).EQ.1) CSGN = -CSGN
      ZT = CMPLX(0.0E0,-FMM)
      RTOL = 1.0E0/TOL
      ASCLE = UFL*RTOL
      DO 120 I=1,NN
C       CY(I) = CY(I)*ZN
C       ZN = ZN*ZT
        ZN=CY(I)
        AA=REAL(ZN)
        BB=AIMAG(ZN)
        ATOL=1.0E0
        IF (MAX(ABS(AA),ABS(BB)).GT.ASCLE) GO TO 125
          ZN = ZN*CMPLX(RTOL,0.0E0)
          ATOL = TOL
  125   CONTINUE
        ZN = ZN*CSGN
        CY(I) = ZN*CMPLX(ATOL,0.0E0)
        CSGN = CSGN*ZT
  120 CONTINUE
      RETURN
  130 CONTINUE
      IF (XN.LT.0.0E0) GO TO 220
      RETURN
  220 CONTINUE
      IERR=2
      NZ=0
      RETURN
  230 CONTINUE
      IF(NW.EQ.(-1)) GO TO 220
      NZ=0
      IERR=5
      RETURN
  240 CONTINUE
      NZ=0
      IERR=4
      RETURN
      END
