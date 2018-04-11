      REAL FUNCTION RJ (X, Y, Z, P, IER)
      CHARACTER*16 XERN3, XERN4, XERN5, XERN6, XERN7
      INTEGER IER
      REAL ALFA, BETA, C1, C2, C3, C4, EA, EB, EC, E2, E3
      REAL LOLIM, UPLIM, EPSLON, ERRTOL
      REAL LAMDA, MU, P, PN, PNDEV
      REAL POWER4, RC, SIGMA, S1, S2, S3, X, XN, XNDEV
      REAL XNROOT, Y, YN, YNDEV, YNROOT, Z, ZN, ZNDEV,
     * ZNROOT
      LOGICAL FIRST
      SAVE ERRTOL,LOLIM,UPLIM,C1,C2,C3,C4,FIRST
      DATA FIRST /.TRUE./
C
C***FIRST EXECUTABLE STATEMENT  RJ
      IF (FIRST) THEN
         ERRTOL = (R1MACH(3)/3.0E0)**(1.0E0/6.0E0)
         LOLIM  = (5.0E0 * R1MACH(1))**(1.0E0/3.0E0)
         UPLIM  = 0.30E0*( R1MACH(2) / 5.0E0)**(1.0E0/3.0E0)
C
         C1 = 3.0E0/14.0E0
         C2 = 1.0E0/3.0E0
         C3 = 3.0E0/22.0E0
         C4 = 3.0E0/26.0E0
      ENDIF
      FIRST = .FALSE.
C
C         CALL ERROR HANDLER IF NECESSARY.
C
      RJ = 0.0E0
      IF (MIN(X,Y,Z).LT.0.0E0) THEN
         IER = 1
         WRITE (XERN3, '(1PE15.6)') X
         WRITE (XERN4, '(1PE15.6)') Y
         WRITE (XERN5, '(1PE15.6)') Z
         CALL XERMSG ('SLATEC', 'RJ',
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
         CALL XERMSG ('SLATEC', 'RJ',
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
      SIGMA = 0.0E0
      POWER4 = 1.0E0
C
   30 MU = (XN+YN+ZN+PN+PN)*0.20E0
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
      SIGMA = SIGMA + POWER4*RC(ALFA,BETA,IER)
      POWER4 = POWER4*0.250E0
      XN = (XN+LAMDA)*0.250E0
      YN = (YN+LAMDA)*0.250E0
      ZN = (ZN+LAMDA)*0.250E0
      PN = (PN+LAMDA)*0.250E0
      GO TO 30
C
   40 EA = XNDEV*(YNDEV+ZNDEV) + YNDEV*ZNDEV
      EB = XNDEV*YNDEV*ZNDEV
      EC = PNDEV*PNDEV
      E2 = EA - 3.0E0*EC
      E3 = EB + 2.0E0*PNDEV*(EA-EC)
      S1 = 1.0E0 + E2*(-C1+0.750E0*C3*E2-1.50E0*C4*E3)
      S2 = EB*(0.50E0*C2+PNDEV*(-C3-C3+PNDEV*C4))
      S3 = PNDEV*EA*(C2-PNDEV*C3) - C2*PNDEV*EC
      RJ = 3.0E0*SIGMA + POWER4*(S1+S2+S3)/(MU* SQRT(MU))
      RETURN
      END
