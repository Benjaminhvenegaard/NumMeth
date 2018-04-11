      SUBROUTINE CACAI (Z, FNU, KODE, MR, N, Y, NZ, RL, TOL, ELIM, ALIM)
      COMPLEX CSGN, CSPN, C1, C2, Y, Z, ZN, CY
      REAL ALIM, ARG, ASCLE, AZ, CPN, DFNU, ELIM, FMR, FNU, PI, RL,
     * SGN, SPN, TOL, YY, R1MACH
      INTEGER INU, IUF, KODE, MR, N, NN, NW, NZ
      DIMENSION Y(N), CY(2)
      DATA PI / 3.14159265358979324E0 /
C***FIRST EXECUTABLE STATEMENT  CACAI
      NZ = 0
      ZN = -Z
      AZ = ABS(Z)
      NN = N
      DFNU = FNU + (N-1)
      IF (AZ.LE.2.0E0) GO TO 10
      IF (AZ*AZ*0.25E0.GT.DFNU+1.0E0) GO TO 20
   10 CONTINUE
C-----------------------------------------------------------------------
C     POWER SERIES FOR THE I FUNCTION
C-----------------------------------------------------------------------
      CALL CSERI(ZN, FNU, KODE, NN, Y, NW, TOL, ELIM, ALIM)
      GO TO 40
   20 CONTINUE
      IF (AZ.LT.RL) GO TO 30
C-----------------------------------------------------------------------
C     ASYMPTOTIC EXPANSION FOR LARGE Z FOR THE I FUNCTION
C-----------------------------------------------------------------------
      CALL CASYI(ZN, FNU, KODE, NN, Y, NW, RL, TOL, ELIM, ALIM)
      IF (NW.LT.0) GO TO 70
      GO TO 40
   30 CONTINUE
C-----------------------------------------------------------------------
C     MILLER ALGORITHM NORMALIZED BY THE SERIES FOR THE I FUNCTION
C-----------------------------------------------------------------------
      CALL CMLRI(ZN, FNU, KODE, NN, Y, NW, TOL)
      IF(NW.LT.0) GO TO 70
   40 CONTINUE
C-----------------------------------------------------------------------
C     ANALYTIC CONTINUATION TO THE LEFT HALF PLANE FOR THE K FUNCTION
C-----------------------------------------------------------------------
      CALL CBKNU(ZN, FNU, KODE, 1, CY, NW, TOL, ELIM, ALIM)
      IF (NW.NE.0) GO TO 70
      FMR = MR
      SGN = -SIGN(PI,FMR)
      CSGN = CMPLX(0.0E0,SGN)
      IF (KODE.EQ.1) GO TO 50
      YY = -AIMAG(ZN)
      CPN = COS(YY)
      SPN = SIN(YY)
      CSGN = CSGN*CMPLX(CPN,SPN)
   50 CONTINUE
C-----------------------------------------------------------------------
C     CALCULATE CSPN=EXP(FNU*PI*I) TO MINIMIZE LOSSES OF SIGNIFICANCE
C     WHEN FNU IS LARGE
C-----------------------------------------------------------------------
      INU = FNU
      ARG = (FNU-INU)*SGN
      CPN = COS(ARG)
      SPN = SIN(ARG)
      CSPN = CMPLX(CPN,SPN)
      IF (MOD(INU,2).EQ.1) CSPN = -CSPN
      C1 = CY(1)
      C2 = Y(1)
      IF (KODE.EQ.1) GO TO 60
      IUF = 0
      ASCLE = 1.0E+3*R1MACH(1)/TOL
      CALL CS1S2(ZN, C1, C2, NW, ASCLE, ALIM, IUF)
      NZ = NZ + NW
   60 CONTINUE
      Y(1) = CSPN*C1 + CSGN*C2
      RETURN
   70 CONTINUE
      NZ = -1
      IF(NW.EQ.(-2)) NZ=-2
      RETURN
      END
