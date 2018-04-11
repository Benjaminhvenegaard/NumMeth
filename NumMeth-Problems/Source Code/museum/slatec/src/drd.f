      DOUBLE PRECISION FUNCTION DRD (X, Y, Z, IER)
      CHARACTER*16 XERN3, XERN4, XERN5, XERN6
      INTEGER IER
      DOUBLE PRECISION LOLIM, TUPLIM, UPLIM, EPSLON, ERRTOL, D1MACH
      DOUBLE PRECISION C1, C2, C3, C4, EA, EB, EC, ED, EF, LAMDA
      DOUBLE PRECISION MU, POWER4, SIGMA, S1, S2, X, XN, XNDEV
      DOUBLE PRECISION XNROOT, Y, YN, YNDEV, YNROOT, Z, ZN, ZNDEV,
     * ZNROOT
      LOGICAL FIRST
      SAVE ERRTOL, LOLIM, UPLIM, C1, C2, C3, C4, FIRST
      DATA FIRST /.TRUE./
C
C***FIRST EXECUTABLE STATEMENT  DRD
      IF (FIRST) THEN
         ERRTOL = (D1MACH(3)/3.0D0)**(1.0D0/6.0D0)
         LOLIM  = 2.0D0/(D1MACH(2))**(2.0D0/3.0D0)
         TUPLIM = D1MACH(1)**(1.0E0/3.0E0)
         TUPLIM = (0.10D0*ERRTOL)**(1.0E0/3.0E0)/TUPLIM
         UPLIM  = TUPLIM**2.0D0
C
         C1 = 3.0D0/14.0D0
         C2 = 1.0D0/6.0D0
         C3 = 9.0D0/22.0D0
         C4 = 3.0D0/26.0D0
      ENDIF
      FIRST = .FALSE.
C
C         CALL ERROR HANDLER IF NECESSARY.
C
      DRD = 0.0D0
      IF( MIN(X,Y).LT.0.0D0) THEN
         IER = 1
         WRITE (XERN3, '(1PE15.6)') X
         WRITE (XERN4, '(1PE15.6)') Y
         CALL XERMSG ('SLATEC', 'DRD',
     *      'MIN(X,Y).LT.0 WHERE X = ' // XERN3 // ' AND Y = ' //
     *      XERN4, 1, 1)
         RETURN
      ENDIF
C
      IF (MAX(X,Y,Z).GT.UPLIM) THEN
         IER = 3
         WRITE (XERN3, '(1PE15.6)') X
         WRITE (XERN4, '(1PE15.6)') Y
         WRITE (XERN5, '(1PE15.6)') Z
         WRITE (XERN6, '(1PE15.6)') UPLIM
         CALL XERMSG ('SLATEC', 'DRD',
     *      'MAX(X,Y,Z).GT.UPLIM WHERE X = ' // XERN3 // ' Y = ' //
     *      XERN4 // ' Z = ' // XERN5 // ' AND UPLIM = ' // XERN6,
     *      3, 1)
         RETURN
      ENDIF
C
      IF (MIN(X+Y,Z).LT.LOLIM) THEN
         IER = 2
         WRITE (XERN3, '(1PE15.6)') X
         WRITE (XERN4, '(1PE15.6)') Y
         WRITE (XERN5, '(1PE15.6)') Z
         WRITE (XERN6, '(1PE15.6)') LOLIM
         CALL XERMSG ('SLATEC', 'DRD',
     *      'MIN(X+Y,Z).LT.LOLIM WHERE X = ' // XERN3 // ' Y = ' //
     *      XERN4 // ' Z = ' // XERN5 // ' AND LOLIM = ' // XERN6,
     *      2, 1)
         RETURN
      ENDIF
C
      IER = 0
      XN = X
      YN = Y
      ZN = Z
      SIGMA = 0.0D0
      POWER4 = 1.0D0
C
   30 MU = (XN+YN+3.0D0*ZN)*0.20D0
      XNDEV = (MU-XN)/MU
      YNDEV = (MU-YN)/MU
      ZNDEV = (MU-ZN)/MU
      EPSLON = MAX(ABS(XNDEV), ABS(YNDEV), ABS(ZNDEV))
      IF (EPSLON.LT.ERRTOL) GO TO 40
      XNROOT = SQRT(XN)
      YNROOT = SQRT(YN)
      ZNROOT = SQRT(ZN)
      LAMDA = XNROOT*(YNROOT+ZNROOT) + YNROOT*ZNROOT
      SIGMA = SIGMA + POWER4/(ZNROOT*(ZN+LAMDA))
      POWER4 = POWER4*0.250D0
      XN = (XN+LAMDA)*0.250D0
      YN = (YN+LAMDA)*0.250D0
      ZN = (ZN+LAMDA)*0.250D0
      GO TO 30
C
   40 EA = XNDEV*YNDEV
      EB = ZNDEV*ZNDEV
      EC = EA - EB
      ED = EA - 6.0D0*EB
      EF = ED + EC + EC
      S1 = ED*(-C1+0.250D0*C3*ED-1.50D0*C4*ZNDEV*EF)
      S2 = ZNDEV*(C2*EF+ZNDEV*(-C3*EC+ZNDEV*C4*EA))
      DRD = 3.0D0*SIGMA + POWER4*(1.0D0+S1+S2)/(MU*SQRT(MU))
C
      RETURN
      END
