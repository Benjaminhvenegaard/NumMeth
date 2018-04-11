      SUBROUTINE ZBESY (ZR, ZI, FNU, KODE, N, CYR, CYI, NZ, CWRKR,
     +   CWRKI, IERR)
C
C     COMPLEX CWRK,CY,C1,C2,EX,HCI,Z,ZU,ZV
      DOUBLE PRECISION CWRKI, CWRKR, CYI, CYR, C1I, C1R, C2I, C2R,
     * ELIM, EXI, EXR, EY, FNU, HCII, STI, STR, TAY, ZI, ZR,
     * D1MACH, ASCLE, RTOL, ATOL, AA, BB, TOL, R1M5
      INTEGER I, IERR, K, KODE, K1, K2, N, NZ, NZ1, NZ2, I1MACH
      DIMENSION CYR(N), CYI(N), CWRKR(N), CWRKI(N)
C***FIRST EXECUTABLE STATEMENT  ZBESY
      IERR = 0
      NZ=0
      IF (ZR.EQ.0.0D0 .AND. ZI.EQ.0.0D0) IERR=1
      IF (FNU.LT.0.0D0) IERR=1
      IF (KODE.LT.1 .OR. KODE.GT.2) IERR=1
      IF (N.LT.1) IERR=1
      IF (IERR.NE.0) RETURN
      HCII = 0.5D0
      CALL ZBESH(ZR, ZI, FNU, KODE, 1, N, CYR, CYI, NZ1, IERR)
      IF (IERR.NE.0.AND.IERR.NE.3) GO TO 170
      CALL ZBESH(ZR, ZI, FNU, KODE, 2, N, CWRKR, CWRKI, NZ2, IERR)
      IF (IERR.NE.0.AND.IERR.NE.3) GO TO 170
      NZ = MIN(NZ1,NZ2)
      IF (KODE.EQ.2) GO TO 60
      DO 50 I=1,N
        STR = CWRKR(I) - CYR(I)
        STI = CWRKI(I) - CYI(I)
        CYR(I) = -STI*HCII
        CYI(I) = STR*HCII
   50 CONTINUE
      RETURN
   60 CONTINUE
      TOL = MAX(D1MACH(4),1.0D-18)
      K1 = I1MACH(15)
      K2 = I1MACH(16)
      K = MIN(ABS(K1),ABS(K2))
      R1M5 = D1MACH(5)
C-----------------------------------------------------------------------
C     ELIM IS THE APPROXIMATE EXPONENTIAL UNDER- AND OVERFLOW LIMIT
C-----------------------------------------------------------------------
      ELIM = 2.303D0*(K*R1M5-3.0D0)
      EXR = COS(ZR)
      EXI = SIN(ZR)
      EY = 0.0D0
      TAY = ABS(ZI+ZI)
      IF (TAY.LT.ELIM) EY = EXP(-TAY)
      IF (ZI.LT.0.0D0) GO TO 90
      C1R = EXR*EY
      C1I = EXI*EY
      C2R = EXR
      C2I = -EXI
   70 CONTINUE
      NZ = 0
      RTOL = 1.0D0/TOL
      ASCLE = D1MACH(1)*RTOL*1.0D+3
      DO 80 I=1,N
C       STR = C1R*CYR(I) - C1I*CYI(I)
C       STI = C1R*CYI(I) + C1I*CYR(I)
C       STR = -STR + C2R*CWRKR(I) - C2I*CWRKI(I)
C       STI = -STI + C2R*CWRKI(I) + C2I*CWRKR(I)
C       CYR(I) = -STI*HCII
C       CYI(I) = STR*HCII
        AA = CWRKR(I)
        BB = CWRKI(I)
        ATOL = 1.0D0
        IF (MAX(ABS(AA),ABS(BB)).GT.ASCLE) GO TO 75
          AA = AA*RTOL
          BB = BB*RTOL
          ATOL = TOL
   75   CONTINUE
        STR = (AA*C2R - BB*C2I)*ATOL
        STI = (AA*C2I + BB*C2R)*ATOL
        AA = CYR(I)
        BB = CYI(I)
        ATOL = 1.0D0
        IF (MAX(ABS(AA),ABS(BB)).GT.ASCLE) GO TO 85
          AA = AA*RTOL
          BB = BB*RTOL
          ATOL = TOL
   85   CONTINUE
        STR = STR - (AA*C1R - BB*C1I)*ATOL
        STI = STI - (AA*C1I + BB*C1R)*ATOL
        CYR(I) = -STI*HCII
        CYI(I) =  STR*HCII
        IF (STR.EQ.0.0D0 .AND. STI.EQ.0.0D0 .AND. EY.EQ.0.0D0) NZ = NZ
     *   + 1
   80 CONTINUE
      RETURN
   90 CONTINUE
      C1R = EXR
      C1I = EXI
      C2R = EXR*EY
      C2I = -EXI*EY
      GO TO 70
  170 CONTINUE
      NZ = 0
      RETURN
      END
