      REAL FUNCTION RD (X, Y, Z, IER)
      CHARACTER*16 XERN3, XERN4, XERN5, XERN6
      INTEGER IER
      REAL LOLIM, UPLIM, EPSLON, ERRTOL
      REAL C1, C2, C3, C4, EA, EB, EC, ED, EF, LAMDA
      REAL MU, POWER4, SIGMA, S1, S2, X, XN, XNDEV
      REAL XNROOT, Y, YN, YNDEV, YNROOT, Z, ZN, ZNDEV, ZNROOT
      LOGICAL FIRST
      SAVE ERRTOL, LOLIM, UPLIM, C1, C2, C3, C4, FIRST
      DATA FIRST /.TRUE./
C
C***FIRST EXECUTABLE STATEMENT  RD
      IF (FIRST) THEN
         ERRTOL = (R1MACH(3)/3.0E0)**(1.0E0/6.0E0)
         LOLIM  = 2.0E0/(R1MACH(2))**(2.0E0/3.0E0)
         TUPLIM = R1MACH(1)**(1.0E0/3.0E0)
         TUPLIM = (0.10E0*ERRTOL)**(1.0E0/3.0E0)/TUPLIM
         UPLIM  = TUPLIM**2.0E0
C
         C1 = 3.0E0/14.0E0
         C2 = 1.0E0/6.0E0
         C3 = 9.0E0/22.0E0
         C4 = 3.0E0/26.0E0
      ENDIF
      FIRST = .FALSE.
C
C         CALL ERROR HANDLER IF NECESSARY.
C
      RD = 0.0E0
      IF( MIN(X,Y).LT.0.0E0) THEN
         IER = 1
         WRITE (XERN3, '(1PE15.6)') X
         WRITE (XERN4, '(1PE15.6)') Y
         CALL XERMSG ('SLATEC', 'RD',
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
         CALL XERMSG ('SLATEC', 'RD',
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
         CALL XERMSG ('SLATEC', 'RD',
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
      SIGMA = 0.0E0
      POWER4 = 1.0E0
C
   30 MU = (XN+YN+3.0E0*ZN)*0.20E0
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
      POWER4 = POWER4*0.250E0
      XN = (XN+LAMDA)*0.250E0
      YN = (YN+LAMDA)*0.250E0
      ZN = (ZN+LAMDA)*0.250E0
      GO TO 30
C
   40 EA = XNDEV*YNDEV
      EB = ZNDEV*ZNDEV
      EC = EA - EB
      ED = EA - 6.0E0*EB
      EF = ED + EC + EC
      S1 = ED*(-C1+0.250E0*C3*ED-1.50E0*C4*ZNDEV*EF)
      S2 = ZNDEV*(C2*EF+ZNDEV*(-C3*EC+ZNDEV*C4*EA))
      RD = 3.0E0*SIGMA + POWER4*(1.0E0+S1+S2)/(MU* SQRT(MU))
C
      RETURN
      END
