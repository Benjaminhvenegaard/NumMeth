      FUNCTION INITDS (OS, NOS, ETA)
      DOUBLE PRECISION OS(*)
C***FIRST EXECUTABLE STATEMENT  INITDS
      IF (NOS .LT. 1) CALL XERMSG ('SLATEC', 'INITDS',
     +   'Number of coefficients is less than 1', 2, 1)
C
      ERR = 0.
      DO 10 II = 1,NOS
        I = NOS + 1 - II
        ERR = ERR + ABS(REAL(OS(I)))
        IF (ERR.GT.ETA) GO TO 20
   10 CONTINUE
C
   20 IF (I .EQ. NOS) CALL XERMSG ('SLATEC', 'INITDS',
     +   'Chebyshev series too short for specified accuracy', 1, 1)
      INITDS = I
C
      RETURN
      END
