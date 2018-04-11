      SUBROUTINE SCHDD (R, LDR, P, X, Z, LDZ, NZ, Y, RHO, C, S, INFO)
      INTEGER LDR,P,LDZ,NZ,INFO
      REAL R(LDR,*),X(*),Z(LDZ,*),Y(*),S(*)
      REAL RHO(*),C(*)
C
      INTEGER I,II,J
      REAL A,ALPHA,AZETA,NORM,SNRM2
      REAL SDOT,T,ZETA,B,XX
C
C     SOLVE THE SYSTEM TRANS(R)*A = X, PLACING THE RESULT
C     IN THE ARRAY S.
C
C***FIRST EXECUTABLE STATEMENT  SCHDD
      INFO = 0
      S(1) = X(1)/R(1,1)
      IF (P .LT. 2) GO TO 20
      DO 10 J = 2, P
         S(J) = X(J) - SDOT(J-1,R(1,J),1,S,1)
         S(J) = S(J)/R(J,J)
   10 CONTINUE
   20 CONTINUE
      NORM = SNRM2(P,S,1)
      IF (NORM .LT. 1.0E0) GO TO 30
         INFO = -1
      GO TO 120
   30 CONTINUE
         ALPHA = SQRT(1.0E0-NORM**2)
C
C        DETERMINE THE TRANSFORMATIONS.
C
         DO 40 II = 1, P
            I = P - II + 1
            SCALE = ALPHA + ABS(S(I))
            A = ALPHA/SCALE
            B = S(I)/SCALE
            NORM = SQRT(A**2+B**2)
            C(I) = A/NORM
            S(I) = B/NORM
            ALPHA = SCALE*NORM
   40    CONTINUE
C
C        APPLY THE TRANSFORMATIONS TO R.
C
         DO 60 J = 1, P
            XX = 0.0E0
            DO 50 II = 1, J
               I = J - II + 1
               T = C(I)*XX + S(I)*R(I,J)
               R(I,J) = C(I)*R(I,J) - S(I)*XX
               XX = T
   50       CONTINUE
   60    CONTINUE
C
C        IF REQUIRED, DOWNDATE Z AND RHO.
C
         IF (NZ .LT. 1) GO TO 110
         DO 100 J = 1, NZ
            ZETA = Y(J)
            DO 70 I = 1, P
               Z(I,J) = (Z(I,J) - S(I)*ZETA)/C(I)
               ZETA = C(I)*ZETA - S(I)*Z(I,J)
   70       CONTINUE
            AZETA = ABS(ZETA)
            IF (AZETA .LE. RHO(J)) GO TO 80
               INFO = 1
               RHO(J) = -1.0E0
            GO TO 90
   80       CONTINUE
               RHO(J) = RHO(J)*SQRT(1.0E0-(AZETA/RHO(J))**2)
   90       CONTINUE
  100    CONTINUE
  110    CONTINUE
  120 CONTINUE
      RETURN
      END
