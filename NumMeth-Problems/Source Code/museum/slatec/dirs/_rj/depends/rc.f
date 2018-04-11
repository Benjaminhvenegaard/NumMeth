      REAL FUNCTION RC (X, Y, IER)
      CHARACTER*16 XERN3, XERN4, XERN5
      INTEGER IER
      REAL C1, C2, ERRTOL, LAMDA, LOLIM
      REAL MU, S, SN, UPLIM, X, XN, Y, YN
      LOGICAL FIRST
      SAVE ERRTOL,LOLIM,UPLIM,C1,C2,FIRST
      DATA FIRST /.TRUE./
C
C***FIRST EXECUTABLE STATEMENT  RC
      IF (FIRST) THEN
         ERRTOL = (R1MACH(3)/16.0E0)**(1.0E0/6.0E0)
         LOLIM  = 5.0E0 * R1MACH(1)
         UPLIM  = R1MACH(2) / 5.0E0
C
         C1 = 1.0E0/7.0E0
         C2 = 9.0E0/22.0E0
      ENDIF
      FIRST = .FALSE.
C
C         CALL ERROR HANDLER IF NECESSARY.
C
      RC = 0.0E0
      IF (X.LT.0.0E0.OR.Y.LE.0.0E0) THEN
         IER = 1
         WRITE (XERN3, '(1PE15.6)') X
         WRITE (XERN4, '(1PE15.6)') Y
         CALL XERMSG ('SLATEC', 'RC',
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
         CALL XERMSG ('SLATEC', 'RC',
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
         CALL XERMSG ('SLATEC', 'RC',
     *      'X+Y.LT.LOLIM WHERE X = ' // XERN3 // ' Y = ' // XERN4 //
     *      ' AND LOLIM = ' // XERN5, 2, 1)
         RETURN
      ENDIF
C
      IER = 0
      XN = X
      YN = Y
C
   30 MU = (XN+YN+YN)/3.0E0
      SN = (YN+MU)/MU - 2.0E0
      IF (ABS(SN).LT.ERRTOL) GO TO 40
      LAMDA = 2.0E0*SQRT(XN)*SQRT(YN) + YN
      XN = (XN+LAMDA)*0.250E0
      YN = (YN+LAMDA)*0.250E0
      GO TO 30
C
   40 S = SN*SN*(0.30E0+SN*(C1+SN*(0.3750E0+SN*C2)))
      RC = (1.0E0+S)/SQRT(MU)
      RETURN
      END
