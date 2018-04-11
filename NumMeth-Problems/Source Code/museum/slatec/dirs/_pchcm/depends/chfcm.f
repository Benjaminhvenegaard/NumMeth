      INTEGER FUNCTION CHFCM (D1, D2, DELTA)
C
C  Fortran intrinsics used:  SIGN.
C  Other routines used:  R1MACH.
C
C ----------------------------------------------------------------------
C
C  Programming notes:
C
C     TEN is actually a tuning parameter, which determines the width of
C     the fuzz around the elliptical boundary.
C
C     To produce a double precision version, simply:
C        a. Change CHFCM to DCHFCM wherever it occurs,
C        b. Change the real declarations to double precision, and
C        c. Change the constants ZERO, ONE, ... to double precision.
C
C  DECLARE ARGUMENTS.
C
      REAL  D1, D2, DELTA
C
C  DECLARE LOCAL VARIABLES.
C
      INTEGER  ISMON, ITRUE
      REAL  A, B, EPS, FOUR, ONE, PHI, TEN, THREE, TWO, ZERO
      SAVE ZERO, ONE, TWO, THREE, FOUR
      SAVE TEN
C
C  INITIALIZE.
C
      DATA  ZERO /0./,  ONE /1.0/,  TWO /2./,  THREE /3./,  FOUR /4./,
     1      TEN /10./
C
C        MACHINE-DEPENDENT PARAMETER -- SHOULD BE ABOUT 10*UROUND.
C***FIRST EXECUTABLE STATEMENT  CHFCM
      EPS = TEN*R1MACH(4)
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
         ITRUE = SIGN (ONE, DELTA)
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
      CHFCM = ISMON
      RETURN
C------------- LAST LINE OF CHFCM FOLLOWS ------------------------------
      END
