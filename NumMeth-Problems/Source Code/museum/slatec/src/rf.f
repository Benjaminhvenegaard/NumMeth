      REAL FUNCTION RF (X, Y, Z, IER)
      CHARACTER*16 XERN3, XERN4, XERN5, XERN6
      INTEGER IER
      REAL LOLIM, UPLIM, EPSLON, ERRTOL
      REAL C1, C2, C3, E2, E3, LAMDA
      REAL MU, S, X, XN, XNDEV
      REAL XNROOT, Y, YN, YNDEV, YNROOT, Z, ZN, ZNDEV,
     * ZNROOT
      LOGICAL FIRST
      SAVE ERRTOL,LOLIM,UPLIM,C1,C2,C3,FIRST
      DATA FIRST /.TRUE./
C
C***FIRST EXECUTABLE STATEMENT  RF
C
      IF (FIRST) THEN
         ERRTOL = (4.0E0*R1MACH(3))**(1.0E0/6.0E0)
         LOLIM  = 5.0E0 * R1MACH(1)
         UPLIM  = R1MACH(2)/5.0E0
C
         C1 = 1.0E0/24.0E0
         C2 = 3.0E0/44.0E0
         C3 = 1.0E0/14.0E0
      ENDIF
      FIRST = .FALSE.
C
C         CALL ERROR HANDLER IF NECESSARY.
C
      RF = 0.0E0
      IF (MIN(X,Y,Z).LT.0.0E0) THEN
         IER = 1
         WRITE (XERN3, '(1PE15.6)') X
         WRITE (XERN4, '(1PE15.6)') Y
         WRITE (XERN5, '(1PE15.6)') Z
         CALL XERMSG ('SLATEC', 'RF',
     *      'MIN(X,Y,Z).LT.0 WHERE X = ' // XERN3 // ' Y = ' // XERN4 //
     *      ' AND Z = ' // XERN5, 1, 1)
         RETURN
      ENDIF
C
      IF (MAX(X,Y,Z).GT.UPLIM) THEN
         IER = 3
         WRITE (XERN3, '(1PE15.6)') X
         WRITE (XERN4, '(1PE15.6)') Y
         WRITE (XERN5, '(1PE15.6)') Z
         WRITE (XERN6, '(1PE15.6)') UPLIM
         CALL XERMSG ('SLATEC', 'RF',
     *      'MAX(X,Y,Z).GT.UPLIM WHERE X = '  // XERN3 // ' Y = ' //
     *      XERN4 // ' Z = ' // XERN5 // ' AND UPLIM = ' // XERN6, 3, 1)
         RETURN
      ENDIF
C
      IF (MIN(X+Y,X+Z,Y+Z).LT.LOLIM) THEN
         IER = 2
         WRITE (XERN3, '(1PE15.6)') X
         WRITE (XERN4, '(1PE15.6)') Y
         WRITE (XERN5, '(1PE15.6)') Z
         WRITE (XERN6, '(1PE15.6)') LOLIM
         CALL XERMSG ('SLATEC', 'RF',
     *      'MIN(X+Y,X+Z,Y+Z).LT.LOLIM WHERE X = ' // XERN3 //
     *      ' Y = ' // XERN4 // ' Z = ' // XERN5 // ' AND LOLIM = ' //
     *      XERN6, 2, 1)
         RETURN
      ENDIF
C
      IER = 0
      XN = X
      YN = Y
      ZN = Z
C
   30 MU = (XN+YN+ZN)/3.0E0
      XNDEV = 2.0E0 - (MU+XN)/MU
      YNDEV = 2.0E0 - (MU+YN)/MU
      ZNDEV = 2.0E0 - (MU+ZN)/MU
      EPSLON = MAX(ABS(XNDEV), ABS(YNDEV), ABS(ZNDEV))
      IF (EPSLON.LT.ERRTOL) GO TO 40
      XNROOT =  SQRT(XN)
      YNROOT =  SQRT(YN)
      ZNROOT =  SQRT(ZN)
      LAMDA = XNROOT*(YNROOT+ZNROOT) + YNROOT*ZNROOT
      XN = (XN+LAMDA)*0.250E0
      YN = (YN+LAMDA)*0.250E0
      ZN = (ZN+LAMDA)*0.250E0
      GO TO 30
C
   40 E2 = XNDEV*YNDEV - ZNDEV*ZNDEV
      E3 = XNDEV*YNDEV*ZNDEV
      S  = 1.0E0 + (C1*E2-0.10E0-C2*E3)*E2 + C3*E3
      RF = S/SQRT(MU)
C
      RETURN
      END
