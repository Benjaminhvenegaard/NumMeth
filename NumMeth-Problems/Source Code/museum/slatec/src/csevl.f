      FUNCTION CSEVL (X, CS, N)
      REAL B0, B1, B2, CS(*), ONEPL, TWOX, X
      LOGICAL FIRST
      SAVE FIRST, ONEPL
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  CSEVL
      IF (FIRST) ONEPL = 1.0E0 + R1MACH(4)
      FIRST = .FALSE.
      IF (N .LT. 1) CALL XERMSG ('SLATEC', 'CSEVL',
     +   'NUMBER OF TERMS .LE. 0', 2, 2)
      IF (N .GT. 1000) CALL XERMSG ('SLATEC', 'CSEVL',
     +   'NUMBER OF TERMS .GT. 1000', 3, 2)
      IF (ABS(X) .GT. ONEPL) CALL XERMSG ('SLATEC', 'CSEVL',
     +   'X OUTSIDE THE INTERVAL (-1,+1)', 1, 1)
C
      B1 = 0.0E0
      B0 = 0.0E0
      TWOX = 2.0*X
      DO 10 I = 1,N
         B2 = B1
         B1 = B0
         NI = N + 1 - I
         B0 = TWOX*B1 - B2 + CS(NI)
   10 CONTINUE
C
      CSEVL = 0.5E0*(B0-B2)
C
      RETURN
      END
