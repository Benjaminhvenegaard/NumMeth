      SUBROUTINE PVALUE (L, NDER, X, YFIT, YP, A)
      DIMENSION YP(*),A(*)
      CHARACTER*8 XERN1, XERN2
C***FIRST EXECUTABLE STATEMENT  PVALUE
      IF (L .LT. 0) GO TO 12
      NDO = MAX(NDER,0)
      NDO = MIN(NDO,L)
      MAXORD = A(1) + 0.5
      K1 = MAXORD + 1
      K2 = K1 + MAXORD
      K3 = K2 + MAXORD + 2
      NORD = A(K3) + 0.5
      IF (L .GT. NORD) GO TO 11
      K4 = K3 + L + 1
      IF (NDER .LT. 1) GO TO 2
      DO 1 I = 1,NDER
 1      YP(I) = 0.0
 2    IF (L .GE. 2) GO TO 4
      IF (L .EQ. 1) GO TO 3
C
C L IS 0
C
      VAL = A(K2+1)
      GO TO 10
C
C L IS 1
C
 3    CC = A(K2+2)
      VAL = A(K2+1) + (X-A(2))*CC
      IF (NDER .GE. 1) YP(1) = CC
      GO TO 10
C
C L IS GREATER THAN 1
C
 4    NDP1 = NDO + 1
      K3P1 = K3 + 1
      K4P1 = K4 + 1
      LP1 = L + 1
      LM1 = L - 1
      ILO = K3 + 3
      IUP = K4 + NDP1
      DO 5 I = ILO,IUP
 5      A(I) = 0.0
      DIF = X - A(LP1)
      KC = K2 + LP1
      A(K4P1) = A(KC)
      A(K3P1) = A(KC-1) + DIF*A(K4P1)
      A(K3+2) = A(K4P1)
C
C EVALUATE RECURRENCE RELATIONS FOR FUNCTION VALUE AND DERIVATIVES
C
      DO 9 I = 1,LM1
        IN = L - I
        INP1 = IN + 1
        K1I = K1 + INP1
        IC = K2 + IN
        DIF = X - A(INP1)
        VAL = A(IC) + DIF*A(K3P1) - A(K1I)*A(K4P1)
        IF (NDO .LE. 0) GO TO 8
        DO 6 N = 1,NDO
          K3PN = K3P1 + N
          K4PN = K4P1 + N
 6        YP(N) = DIF*A(K3PN) + N*A(K3PN-1) - A(K1I)*A(K4PN)
C
C SAVE VALUES NEEDED FOR NEXT EVALUATION OF RECURRENCE RELATIONS
C
        DO 7 N = 1,NDO
          K3PN = K3P1 + N
          K4PN = K4P1 + N
          A(K4PN) = A(K3PN)
 7        A(K3PN) = YP(N)
 8      A(K4P1) = A(K3P1)
 9      A(K3P1) = VAL
C
C NORMAL RETURN OR ABORT DUE TO ERROR
C
 10   YFIT = VAL
      RETURN
C
   11 WRITE (XERN1, '(I8)') L
      WRITE (XERN2, '(I8)') NORD
      CALL XERMSG ('SLATEC', 'PVALUE',
     *   'THE ORDER OF POLYNOMIAL EVALUATION, L = ' // XERN1 //
     *   ' REQUESTED EXCEEDS THE HIGHEST ORDER FIT, NORD = ' // XERN2 //
     *   ', COMPUTED BY POLFIT -- EXECUTION TERMINATED.', 8, 2)
      RETURN
C
   12 CALL XERMSG ('SLATEC', 'PVALUE',
     +   'INVALID INPUT PARAMETER.  ORDER OF POLYNOMIAL EVALUATION ' //
     +   'REQUESTED IS NEGATIVE -- EXECUTION TERMINATED.', 2, 2)
      RETURN
      END
