      DOUBLE PRECISION FUNCTION DPOCH (A, X)
      DOUBLE PRECISION A, X, ABSA, ABSAX, ALNGA, ALNGAX, AX, B, PI,
     1  SGNGA, SGNGAX, DFAC, DLNREL, D9LGMC, DGAMMA, DGAMR, DCOT
      EXTERNAL DGAMMA
      SAVE PI
      DATA PI / 3.1415926535 8979323846 2643383279 503 D0 /
C***FIRST EXECUTABLE STATEMENT  DPOCH
      AX = A + X
      IF (AX.GT.0.0D0) GO TO 30
      IF (AINT(AX).NE.AX) GO TO 30
C
      IF (A .GT. 0.0D0 .OR. AINT(A) .NE. A) CALL XERMSG ('SLATEC',
     +   'DPOCH', 'A+X IS NON-POSITIVE INTEGER BUT A IS NOT', 2, 2)
C
C WE KNOW HERE THAT BOTH A+X AND A ARE NON-POSITIVE INTEGERS.
C
      DPOCH = 1.0D0
      IF (X.EQ.0.D0) RETURN
C
      N = X
      IF (MIN(A+X,A).LT.(-20.0D0)) GO TO 20
C
      IA = A
      DPOCH = (-1.0D0)**N * DFAC(-IA)/DFAC(-IA-N)
      RETURN
C
 20   DPOCH = (-1.0D0)**N * EXP ((A-0.5D0)*DLNREL(X/(A-1.0D0))
     1  + X*LOG(-A+1.0D0-X) - X + D9LGMC(-A+1.0D0) - D9LGMC(-A-X+1.D0))
      RETURN
C
C A+X IS NOT ZERO OR A NEGATIVE INTEGER.
C
 30   DPOCH = 0.0D0
      IF (A.LE.0.0D0 .AND. AINT(A).EQ.A) RETURN
C
      N = ABS(X)
      IF (DBLE(N).NE.X .OR. N.GT.20) GO TO 50
C
C X IS A SMALL NON-POSITIVE INTEGER, PRESUMMABLY A COMMON CASE.
C
      DPOCH = 1.0D0
      IF (N.EQ.0) RETURN
      DO 40 I=1,N
        DPOCH = DPOCH * (A+I-1)
 40   CONTINUE
      RETURN
C
 50   ABSAX = ABS(A+X)
      ABSA = ABS(A)
      IF (MAX(ABSAX,ABSA).GT.20.0D0) GO TO 60
      DPOCH = DGAMMA(A+X) * DGAMR(A)
      RETURN
C
 60   IF (ABS(X).GT.0.5D0*ABSA) GO TO 70
C
C ABS(X) IS SMALL AND BOTH ABS(A+X) AND ABS(A) ARE LARGE.  THUS,
C A+X AND A MUST HAVE THE SAME SIGN.  FOR NEGATIVE A, WE USE
C GAMMA(A+X)/GAMMA(A) = GAMMA(-A+1)/GAMMA(-A-X+1) *
C SIN(PI*A)/SIN(PI*(A+X))
C
      B = A
      IF (B.LT.0.0D0) B = -A - X + 1.0D0
      DPOCH = EXP ((B-0.5D0)*DLNREL(X/B) + X*LOG(B+X) - X
     1  + D9LGMC(B+X) - D9LGMC(B) )
      IF (A.LT.0.0D0 .AND. DPOCH.NE.0.0D0) DPOCH =
     1  DPOCH/(COS(PI*X) + DCOT(PI*A)*SIN(PI*X) )
      RETURN
C
 70   CALL DLGAMS (A+X, ALNGAX, SGNGAX)
      CALL DLGAMS (A, ALNGA, SGNGA)
      DPOCH = SGNGAX * SGNGA * EXP(ALNGAX-ALNGA)
C
      RETURN
      END