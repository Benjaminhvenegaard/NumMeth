      SUBROUTINE DSOS (FNC, NEQ, X, RTOLX, ATOLX, TOLF, IFLAG, RW, LRW,
     +   IW, LIW)
      INTEGER IFLAG, INPFLG, IPRINT, IW(*), K1, K2, K3, K4, K5, K6,
     1     LIW, LRW, MXIT, NC, NCJS, NEQ, NSRI, NSRRC
      DOUBLE PRECISION ATOLX, FNC, RTOLX, RW(*), TOLF, X(*)
      CHARACTER*8 XERN1
      CHARACTER*16 XERN3, XERN4
      EXTERNAL FNC
C***FIRST EXECUTABLE STATEMENT  DSOS
      INPFLG = IFLAG
C
C     CHECK FOR VALID INPUT
C
      IF (NEQ .LE. 0) THEN
         WRITE (XERN1, '(I8)') NEQ
         CALL XERMSG ('SLATEC', 'DSOS', 'THE NUMBER OF EQUATIONS ' //
     *      'MUST BE A POSITIVE INTEGER.  YOU HAVE CALLED THE ' //
     *      'CODE WITH NEQ = ' // XERN1, 1, 1)
         IFLAG = 9
      ENDIF
C
      IF (RTOLX .LT. 0.0D0 .OR. ATOLX .LT. 0.0D0) THEN
         WRITE (XERN3, '(1PE15.6)') ATOLX
         WRITE (XERN4, '(1PE15.6)') RTOLX
         CALL XERMSG ('SLATEC', 'DSOS', 'THE ERROR TOLERANCES FOR ' //
     *      'THE SOLUTION ITERATES CANNOT BE NEGATIVE. YOU HAVE ' //
     *      'CALLED THE CODE WITH  RTOLX = ' // XERN3 //
     *      ' AND ATOLX = ' // XERN4,2, 1)
            IFLAG = 9
      ENDIF
C
      IF (TOLF .LT. 0.0D0) THEN
         WRITE (XERN3, '(1PE15.6)') TOLF
         CALL XERMSG ('SLATEC', 'DSOS', 'THE RESIDUAL ERROR ' //
     *      'TOLERANCE MUST BE NON-NEGATIVE.  YOU HAVE CALLED THE ' //
     *      'CODE WITH TOLF = ' // XERN3, 3, 1)
            IFLAG = 9
      ENDIF
C
      IPRINT = 0
      MXIT = 50
      IF (INPFLG .EQ. (-1)) THEN
         IF (IW(1) .EQ. (-1)) IPRINT = -1
         MXIT = IW(2)
         IF (MXIT .LE. 0) THEN
            WRITE (XERN1, '(I8)') MXIT
            CALL XERMSG ('SLATEC', 'DSOS', 'YOU HAVE TOLD THE CODE ' //
     *         'TO USE OPTIONAL INPUT ITEMS BY SETTING IFLAG=-1. ' //
     *         'HOWEVER YOU HAVE CALLED THE CODE WITH THE MAXIMUM ' //
     *         'ALLOWABLE NUMBER OF ITERATIONS SET TO IW(2) = ' //
     *         XERN1, 4, 1)
            IFLAG = 9
         ENDIF
      ENDIF
C
      NC = (NEQ*(NEQ+1))/2
      IF (LRW .LT. 1 + 6*NEQ + NC) THEN
         WRITE (XERN1, '(I8)') LRW
         CALL XERMSG ('SLATEC', 'DSOS', 'DIMENSION OF THE RW ARRAY ' //
     *      'MUST BE AT LEAST 1 + 6*NEQ + NEQ*(NEQ+1)/2 .  YOU HAVE ' //
     *      'CALLED THE CODE WITH LRW = ' // XERN1, 5, 1)
         IFLAG = 9
      ENDIF
C
      IF (LIW .LT. 3 + NEQ) THEN
         WRITE (XERN1, '(I8)') LIW
         CALL XERMSG ('SLATEC', 'DSOS', 'DIMENSION OF THE IW ARRAY ' //
     *      'MUST BE AT LEAST 3 + NEQ.  YOU HAVE CALLED THE CODE ' //
     *      'WITH  LIW = ' // XERN1, 6, 1)
         IFLAG = 9
      ENDIF
C
      IF (IFLAG .NE. 9) THEN
         NCJS = 6
         NSRRC = 4
         NSRI = 5
C
         K1 = NC + 2
         K2 = K1 + NEQ
         K3 = K2 + NEQ
         K4 = K3 + NEQ
         K5 = K4 + NEQ
         K6 = K5 + NEQ
C
         CALL DSOSEQ(FNC, NEQ, X, RTOLX, ATOLX, TOLF, IFLAG, MXIT, NCJS,
     1               NSRRC, NSRI, IPRINT, RW(1), RW(2), NC, RW(K1),
     2               RW(K2), RW(K3), RW(K4), RW(K5), RW(K6), IW(4))
C
         IW(3) = MXIT
      ENDIF
      RETURN
      END
