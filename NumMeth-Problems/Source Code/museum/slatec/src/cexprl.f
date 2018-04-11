      COMPLEX FUNCTION CEXPRL (Z)
      COMPLEX Z
      LOGICAL FIRST
      SAVE NTERMS, RBND, FIRST
      DATA FIRST / .TRUE. /
C***FIRST EXECUTABLE STATEMENT  CEXPRL
      IF (FIRST) THEN
         ALNEPS = LOG(R1MACH(3))
         XN = 3.72 - 0.3*ALNEPS
         XLN = LOG((XN+1.0)/1.36)
         NTERMS = XN - (XN*XLN+ALNEPS)/(XLN+1.36) + 1.5
         RBND = R1MACH(3)
      ENDIF
      FIRST = .FALSE.
C
      R = ABS(Z)
      IF (R.GT.0.5) CEXPRL = (EXP(Z) - 1.0) / Z
      IF (R.GT.0.5) RETURN
C
      CEXPRL = (1.0, 0.0)
      IF (R.LT.RBND) RETURN
C
      CEXPRL = (0.0, 0.0)
      DO 20 I=1,NTERMS
        CEXPRL = 1.0 + CEXPRL*Z/(NTERMS+2-I)
 20   CONTINUE
C
      RETURN
      END
