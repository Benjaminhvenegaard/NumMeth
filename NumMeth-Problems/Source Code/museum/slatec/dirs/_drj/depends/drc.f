      DOUBLE PRECISION FUNCTION DRC (X, Y, IER)
      CHARACTER*16 XERN3, XERN4, XERN5
      INTEGER IER
      DOUBLE PRECISION C1, C2, ERRTOL, LAMDA, LOLIM, D1MACH
      DOUBLE PRECISION MU, S, SN, UPLIM, X, XN, Y, YN
      LOGICAL FIRST
      SAVE ERRTOL,LOLIM,UPLIM,C1,C2,FIRST
      DATA FIRST /.TRUE./
C
C***FIRST EXECUTABLE STATEMENT  DRC
      IF (FIRST) THEN
         ERRTOL = (D1MACH(3)/16.0D0)**(1.0D0/6.0D0)
         LOLIM  = 5.0D0 * D1MACH(1)
         UPLIM  = D1MACH(2) / 5.0D0
C
         C1 = 1.0D0/7.0D0
         C2 = 9.0D0/22.0D0
      ENDIF
      FIRST = .FALSE.
C
C         CALL ERROR HANDLER IF NECESSARY.
C
      DRC = 0.0D0
      IF (X.LT.0.0D0.OR.Y.LE.0.0D0) THEN
         IER = 1
         WRITE (XERN3, '(1PE15.6)') X
         WRITE (XERN4, '(1PE15.6)') Y
         CALL XERMSG ('SLATEC', 'DRC',
     *      'X.LT.0 .OR. Y.LE.0 WHERE X = ' // XERN3 // ' AND Y = ' //
     *      XERN4, 1, 1)
         RETURN
      ENDIF
C
      IF (MAX(X,Y).GT.UPLIM) THEN
         IER = 3
         WRITE (XERN3, '(1PE15.6)') X
         WRITE (XERN4, '(1PE15.6)') Y
         WRITE (XERN5, '(1PE15.6)') UPLIM
         CALL XERMSG ('SLATEC', 'DRC',
     *      'MAX(X,Y).GT.UPLIM WHERE X = '  // XERN3 // ' Y = ' //
     *      XERN4 // ' AND UPLIM = ' // XERN5, 3, 1)
         RETURN
      ENDIF
C
      IF (X+Y.LT.LOLIM) THEN
         IER = 2
         WRITE (XERN3, '(1PE15.6)') X
         WRITE (XERN4, '(1PE15.6)') Y
         WRITE (XERN5, '(1PE15.6)') LOLIM
         CALL XERMSG ('SLATEC', 'DRC',
     *      'X+Y.LT.LOLIM WHERE X = ' // XERN3 // ' Y = ' // XERN4 //
     *      ' AND LOLIM = ' // XERN5, 2, 1)
         RETURN
      ENDIF
C
      IER = 0
      XN = X
      YN = Y
C
   30 MU = (XN+YN+YN)/3.0D0
      SN = (YN+MU)/MU - 2.0D0
      IF (ABS(SN).LT.ERRTOL) GO TO 40
      LAMDA = 2.0D0*SQRT(XN)*SQRT(YN) + YN
      XN = (XN+LAMDA)*0.250D0
      YN = (YN+LAMDA)*0.250D0
      GO TO 30
C
   40 S = SN*SN*(0.30D0+SN*(C1+SN*(0.3750D0+SN*C2)))
      DRC = (1.0D0+S)/SQRT(MU)
      RETURN
      END
