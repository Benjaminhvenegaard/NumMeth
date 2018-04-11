      DOUBLE PRECISION FUNCTION DCHFIE (X1, X2, F1, F2, D1, D2, A, B)
C
C  Programming notes:
C  1. There is no error return from this routine because zero is
C     indeed the mathematically correct answer when X1.EQ.X2 .
C**End
C
C  DECLARE ARGUMENTS.
C
      DOUBLE PRECISION  X1, X2, F1, F2, D1, D2, A, B
C
C  DECLARE LOCAL VARIABLES.
C
      DOUBLE PRECISION  DTERM, FOUR, FTERM, H, HALF, PHIA1, PHIA2,
     *      PHIB1, PHIB2, PSIA1, PSIA2, PSIB1, PSIB2, SIX, TA1, TA2,
     *      TB1, TB2, THREE, TWO, UA1, UA2, UB1, UB2
      SAVE HALF, TWO, THREE, FOUR, SIX
C
C  INITIALIZE.
C
      DATA  HALF/.5D0/, TWO/2.D0/, THREE/3.D0/, FOUR/4.D0/, SIX/6.D0/
C
C  VALIDITY CHECK INPUT.
C
C***FIRST EXECUTABLE STATEMENT  DCHFIE
      IF (X1 .EQ. X2)  THEN
         DCHFIE = 0
      ELSE
         H = X2 - X1
         TA1 = (A - X1) / H
         TA2 = (X2 - A) / H
         TB1 = (B - X1) / H
         TB2 = (X2 - B) / H
C
         UA1 = TA1**3
         PHIA1 = UA1 * (TWO - TA1)
         PSIA1 = UA1 * (THREE*TA1 - FOUR)
         UA2 = TA2**3
         PHIA2 =  UA2 * (TWO - TA2)
         PSIA2 = -UA2 * (THREE*TA2 - FOUR)
C
         UB1 = TB1**3
         PHIB1 = UB1 * (TWO - TB1)
         PSIB1 = UB1 * (THREE*TB1 - FOUR)
         UB2 = TB2**3
         PHIB2 =  UB2 * (TWO - TB2)
         PSIB2 = -UB2 * (THREE*TB2 - FOUR)
C
         FTERM =   F1*(PHIA2 - PHIB2) + F2*(PHIB1 - PHIA1)
         DTERM = ( D1*(PSIA2 - PSIB2) + D2*(PSIB1 - PSIA1) )*(H/SIX)
C
         DCHFIE = (HALF*H) * (FTERM + DTERM)
      ENDIF
C
      RETURN
C------------- LAST LINE OF DCHFIE FOLLOWS -----------------------------
      END
