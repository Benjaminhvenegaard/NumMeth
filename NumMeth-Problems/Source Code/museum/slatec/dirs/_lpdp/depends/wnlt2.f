      LOGICAL FUNCTION WNLT2 (ME, MEND, IR, FACTOR, TAU, SCALE, WIC)
      REAL             FACTOR, SCALE(*), TAU, WIC(*)
      INTEGER IR, ME, MEND
C
      REAL             RN, SN, T
      INTEGER J
C
C***FIRST EXECUTABLE STATEMENT  WNLT2
      SN = 0.E0
      RN = 0.E0
      DO 10 J=1,MEND
         T = SCALE(J)
         IF (J.LE.ME) T = T/FACTOR
         T = T*WIC(J)**2
C
         IF (J.LT.IR) THEN
            SN = SN + T
         ELSE
            RN = RN + T
         ENDIF
   10 CONTINUE
      WNLT2 = RN .GT. SN*TAU**2
      RETURN
      END
