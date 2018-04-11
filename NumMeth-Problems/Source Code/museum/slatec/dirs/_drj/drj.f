      DOUBLE PRECISION FUNCTION DRJ (X, Y, Z, P, IER)
      INTEGER IER
      CHARACTER*16 XERN3, XERN4, XERN5, XERN6, XERN7
      DOUBLE PRECISION ALFA, BETA, C1, C2, C3, C4, EA, EB, EC, E2, E3
      DOUBLE PRECISION LOLIM, UPLIM, EPSLON, ERRTOL, D1MACH
      DOUBLE PRECISION LAMDA, MU, P, PN, PNDEV
      DOUBLE PRECISION POWER4, DRC, SIGMA, S1, S2, S3, X, XN, XNDEV
      DOUBLE PRECISION XNROOT, Y, YN, YNDEV, YNROOT, Z, ZN, ZNDEV,
     * ZNROOT
      LOGICAL FIRST
      SAVE ERRTOL,LOLIM,UPLIM,C1,C2,C3,C4,FIRST
      DATA FIRST /.TRUE./
C
C***FIRST EXECUTABLE STATEMENT  DRJ
      IF (FIRST) THEN
         ERRTOL = (D1MACH(3)/3.0D0)**(1.0D0/6.0D0)
         LOLIM  = (5.0D0 * D1MACH(1))**(1.0D0/3.0D0)
         UPLIM  = 0.30D0*( D1MACH(2) / 5.0D0)**(1.0D0/3.0D0)
C
         C1 = 3.0D0/14.0D0
         C2 = 1.0D0/3.0D0
         C3 = 3.0D0/22.0D0
         C4 = 3.0D0/26.0D0
      ENDIF
      FIRST = .FALSE.
C
C         CALL ERROR HANDLER IF NECESSARY.
C
      DRJ = 0.0D0
      IF (MIN(X,Y,Z).LT.0.0D0) THEN
         IER = 1
         WRITE (XERN3, '(1PE15.6)') X
         WRITE (XERN4, '(1PE15.6)') Y
         WRITE (XERN5, '(1PE15.6)') Z
         CALL XERMSG ('SLATEC', 'DRJ',
     *      'MIN(X,Y,Z).LT.0 WHERE X = ' // XERN3 // ' Y = ' // XERN4 //
     *      ' AND Z = ' // XERN5, 1, 1)
         RETURN
      ENDIF
C
      IF (MAX(X,Y,Z,P).GT.UPLIM) THEN
         IER = 3
         WRITE (XERN3, '(1PE15.6)') X
         WRITE (XERN4, '(1PE15.6)') Y
         WRITE (XERN5, '(1PE15.6)') Z
         WRITE (XERN6, '(1PE15.6)') P
         WRITE (XERN7, '(1PE15.6)') UPLIM
         CALL XERMSG ('SLATEC', 'DRJ',
     *      'MAX(X,Y,Z,P).GT.UPLIM WHERE X = ' // XERN3 // ' Y = ' //
     *      XERN4 // ' Z = ' // XERN5 // ' P = ' // XERN6 //
     *      ' AND UPLIM = ' // XERN7, 3, 1)
         RETURN
      ENDIF
C
      IF (MIN(X+Y,X+Z,Y+Z,P).LT.LOLIM) THEN
         IER = 2
         WRITE (XERN3, '(1PE15.6)') X
         WRITE (XERN4, '(1PE15.6)') Y
         WRITE (XERN5, '(1PE15.6)') Z
         WRITE (XERN6, '(1PE15.6)') P
         WRITE (XERN7, '(1PE15.6)') LOLIM
         CALL XERMSG ('SLATEC', 'RJ',
     *      'MIN(X+Y,X+Z,Y+Z,P).LT.LOLIM WHERE X = ' // XERN3 //
     *      ' Y = ' // XERN4 // ' Z = '  // XERN5 // ' P = ' // XERN6 //
     *      ' AND LOLIM = ', 2, 1)
         RETURN
      ENDIF
C
      IER = 0
      XN = X
      YN = Y
      ZN = Z
      PN = P
      SIGMA = 0.0D0
      POWER4 = 1.0D0
C
   30 MU = (XN+YN+ZN+PN+PN)*0.20D0
      XNDEV = (MU-XN)/MU
      YNDEV = (MU-YN)/MU
      ZNDEV = (MU-ZN)/MU
      PNDEV = (MU-PN)/MU
      EPSLON = MAX(ABS(XNDEV), ABS(YNDEV), ABS(ZNDEV), ABS(PNDEV))
      IF (EPSLON.LT.ERRTOL) GO TO 40
      XNROOT =  SQRT(XN)
      YNROOT =  SQRT(YN)
      ZNROOT =  SQRT(ZN)
      LAMDA = XNROOT*(YNROOT+ZNROOT) + YNROOT*ZNROOT
      ALFA = PN*(XNROOT+YNROOT+ZNROOT) + XNROOT*YNROOT*ZNROOT
      ALFA = ALFA*ALFA
      BETA = PN*(PN+LAMDA)*(PN+LAMDA)
      SIGMA = SIGMA + POWER4*DRC(ALFA,BETA,IER)
      POWER4 = POWER4*0.250D0
      XN = (XN+LAMDA)*0.250D0
      YN = (YN+LAMDA)*0.250D0
      ZN = (ZN+LAMDA)*0.250D0
      PN = (PN+LAMDA)*0.250D0
      GO TO 30
C
   40 EA = XNDEV*(YNDEV+ZNDEV) + YNDEV*ZNDEV
      EB = XNDEV*YNDEV*ZNDEV
      EC = PNDEV*PNDEV
      E2 = EA - 3.0D0*EC
      E3 = EB + 2.0D0*PNDEV*(EA-EC)
      S1 = 1.0D0 + E2*(-C1+0.750D0*C3*E2-1.50D0*C4*E3)
      S2 = EB*(0.50D0*C2+PNDEV*(-C3-C3+PNDEV*C4))
      S3 = PNDEV*EA*(C2-PNDEV*C3) - C2*PNDEV*EC
      DRJ = 3.0D0*SIGMA + POWER4*(S1+S2+S3)/(MU* SQRT(MU))
      RETURN
      END
