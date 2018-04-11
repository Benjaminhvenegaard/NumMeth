      INTEGER FUNCTION DCHFCM (D1, D2, DELTA)
C
C  Fortran intrinsics used:  DSIGN.
C  Other routines used:  D1MACH.
C
C ----------------------------------------------------------------------
C
C  Programming notes:
C
C     TEN is actually a tuning parameter, which determines the width of
C     the fuzz around the elliptical boundary.
C
C     To produce a single precision version, simply:
C        a. Change DCHFCM to CHFCM wherever it occurs,
C        b. Change the double precision declarations to real, and
C        c. Change the constants ZERO, ONE, ... to single precision.
C
C  DECLARE ARGUMENTS.
C
      DOUBLE PRECISION  D1, D2, DELTA, D1MACH
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER ISMON, ITRUE
      DOUBLE PRECISION  A, B, EPS, FOUR, ONE, PHI, TEN, THREE, TWO,
     * ZERO
      SAVE ZERO, ONE, TWO, THREE, FOUR
      SAVE TEN
C
C  INITIALIZE.
C
      DATA ZERO /0.D0/, ONE/1.D0/, TWO/2.D0/, THREE/3.D0/, FOUR/4.D0/,
     1      TEN /10.D0/
C
C        MACHINE-DEPENDENT PARAMETER -- SHOULD BE ABOUT 10*UROUND.
C***FIRST EXECUTABLE STATEMENT  DCHFCM
      EPS = TEN*D1MACH(4)
C
C  MAKE THE CHECK.
C
      IF (DELTA .EQ. ZERO)  THEN
C        CASE OF CONSTANT DATA.
         IF ((D1.EQ.ZERO) .AND. (D2.EQ.ZERO))  THEN
            ISMON = 0
         ELSE
            ISMON = 2
         ENDIF
      ELSE
C        DATA IS NOT CONSTANT -- PICK UP SIGN.
         ITRUE = DSIGN (ONE, DELTA)
         A = D1/DELTA
         B = D2/DELTA
         IF ((A.LT.ZERO) .OR. (B.LT.ZERO))  THEN
            ISMON = 2
         ELSE IF ((A.LE.THREE-EPS) .AND. (B.LE.THREE-EPS))  THEN
C           INSIDE SQUARE (0,3)X(0,3)  IMPLIES   OK.
            ISMON = ITRUE
         ELSE IF ((A.GT.FOUR+EPS) .AND. (B.GT.FOUR+EPS))  THEN
C           OUTSIDE SQUARE (0,4)X(0,4)  IMPLIES   NONMONOTONIC.
            ISMON = 2
         ELSE
C           MUST CHECK AGAINST BOUNDARY OF ELLIPSE.
            A = A - TWO
            B = B - TWO
            PHI = ((A*A + B*B) + A*B) - THREE
            IF (PHI .LT. -EPS)  THEN
               ISMON = ITRUE
            ELSE IF (PHI .GT. EPS)  THEN
               ISMON = 2
            ELSE
C              TO CLOSE TO BOUNDARY TO TELL,
C                  IN THE PRESENCE OF ROUND-OFF ERRORS.
               ISMON = 3*ITRUE
            ENDIF
         ENDIF
      ENDIF
C
C  RETURN VALUE.
C
      DCHFCM = ISMON
      RETURN
C------------- LAST LINE OF DCHFCM FOLLOWS -----------------------------
      END
