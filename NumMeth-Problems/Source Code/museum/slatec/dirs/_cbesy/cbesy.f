      SUBROUTINE CBESY (Z, FNU, KODE, N, CY, NZ, CWRK, IERR)
C
      COMPLEX CWRK, CY, C1, C2, EX, HCI, Z, ZU, ZV
      REAL ELIM, EY, FNU, R1, R2, TAY, XX, YY, R1MACH, R1M5, ASCLE,
     *  RTOL, ATOL, TOL, AA, BB
      INTEGER I, IERR, K, KODE, K1, K2, N, NZ, NZ1, NZ2, I1MACH
      DIMENSION CY(N), CWRK(N)
C***FIRST EXECUTABLE STATEMENT  CBESY
      XX = REAL(Z)
      YY = AIMAG(Z)
      IERR = 0
      NZ=0
      IF (XX.EQ.0.0E0 .AND. YY.EQ.0.0E0) IERR=1
      IF (FNU.LT.0.0E0) IERR=1
      IF (KODE.LT.1 .OR. KODE.GT.2) IERR=1
      IF (N.LT.1) IERR=1
      IF (IERR.NE.0) RETURN
      HCI = CMPLX(0.0E0,0.5E0)
      CALL CBESH(Z, FNU, KODE, 1, N, CY, NZ1, IERR)
      IF (IERR.NE.0.AND.IERR.NE.3) GO TO 170
      CALL CBESH(Z, FNU, KODE, 2, N, CWRK, NZ2, IERR)
      IF (IERR.NE.0.AND.IERR.NE.3) GO TO 170
      NZ = MIN(NZ1,NZ2)
      IF (KODE.EQ.2) GO TO 60
      DO 50 I=1,N
        CY(I) = HCI*(CWRK(I)-CY(I))
   50 CONTINUE
      RETURN
   60 CONTINUE
      TOL = MAX(R1MACH(4),1.0E-18)
      K1 = I1MACH(12)
      K2 = I1MACH(13)
      K = MIN(ABS(K1),ABS(K2))
      R1M5 = R1MACH(5)
C-----------------------------------------------------------------------
C     ELIM IS THE APPROXIMATE EXPONENTIAL UNDER- AND OVERFLOW LIMIT
C-----------------------------------------------------------------------
      ELIM = 2.303E0*(K*R1M5-3.0E0)
      R1 = COS(XX)
      R2 = SIN(XX)
      EX = CMPLX(R1,R2)
      EY = 0.0E0
      TAY = ABS(YY+YY)
      IF (TAY.LT.ELIM) EY = EXP(-TAY)
      IF (YY.LT.0.0E0) GO TO 90
      C1 = EX*CMPLX(EY,0.0E0)
      C2 = CONJG(EX)
   70 CONTINUE
      NZ = 0
      RTOL = 1.0E0/TOL
      ASCLE = R1MACH(1)*RTOL*1.0E+3
      DO 80 I=1,N
C       CY(I) = HCI*(C2*CWRK(I)-C1*CY(I))
        ZV = CWRK(I)
        AA=REAL(ZV)
        BB=AIMAG(ZV)
        ATOL=1.0E0
        IF (MAX(ABS(AA),ABS(BB)).GT.ASCLE) GO TO 75
          ZV = ZV*CMPLX(RTOL,0.0E0)
          ATOL = TOL
   75   CONTINUE
        ZV = ZV*C2*HCI
        ZV = ZV*CMPLX(ATOL,0.0E0)
        ZU=CY(I)
        AA=REAL(ZU)
        BB=AIMAG(ZU)
        ATOL=1.0E0
        IF (MAX(ABS(AA),ABS(BB)).GT.ASCLE) GO TO 85
          ZU = ZU*CMPLX(RTOL,0.0E0)
          ATOL = TOL
   85   CONTINUE
        ZU = ZU*C1*HCI
        ZU = ZU*CMPLX(ATOL,0.0E0)
        CY(I) = ZV - ZU
        IF (CY(I).EQ.CMPLX(0.0E0,0.0E0) .AND. EY.EQ.0.0E0) NZ = NZ + 1
   80 CONTINUE
      RETURN
   90 CONTINUE
      C1 = EX
      C2 = CONJG(EX)*CMPLX(EY,0.0E0)
      GO TO 70
  170 CONTINUE
      NZ = 0
      RETURN
      END
