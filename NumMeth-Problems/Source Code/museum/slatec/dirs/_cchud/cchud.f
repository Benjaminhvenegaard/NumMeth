      SUBROUTINE CCHUD (R, LDR, P, X, Z, LDZ, NZ, Y, RHO, C, S)
      INTEGER LDR,P,LDZ,NZ
      REAL RHO(*),C(*)
      COMPLEX R(LDR,*),X(*),Z(LDZ,*),Y(*),S(*)
C
      INTEGER I,J,JM1
      REAL AZETA,SCALE
      COMPLEX T,XJ,ZETA
C
C     UPDATE R.
C
C***FIRST EXECUTABLE STATEMENT  CCHUD
      DO 30 J = 1, P
         XJ = X(J)
C
C        APPLY THE PREVIOUS ROTATIONS.
C
         JM1 = J - 1
         IF (JM1 .LT. 1) GO TO 20
         DO 10 I = 1, JM1
            T = C(I)*R(I,J) + S(I)*XJ
            XJ = C(I)*XJ - CONJG(S(I))*R(I,J)
            R(I,J) = T
   10    CONTINUE
   20    CONTINUE
C
C        COMPUTE THE NEXT ROTATION.
C
         CALL CROTG(R(J,J),XJ,C(J),S(J))
   30 CONTINUE
C
C     IF REQUIRED, UPDATE Z AND RHO.
C
      IF (NZ .LT. 1) GO TO 70
      DO 60 J = 1, NZ
         ZETA = Y(J)
         DO 40 I = 1, P
            T = C(I)*Z(I,J) + S(I)*ZETA
            ZETA = C(I)*ZETA - CONJG(S(I))*Z(I,J)
            Z(I,J) = T
   40    CONTINUE
         AZETA = ABS(ZETA)
         IF (AZETA .EQ. 0.0E0 .OR. RHO(J) .LT. 0.0E0) GO TO 50
            SCALE = AZETA + RHO(J)
            RHO(J) = SCALE*SQRT((AZETA/SCALE)**2+(RHO(J)/SCALE)**2)
   50    CONTINUE
   60 CONTINUE
   70 CONTINUE
      RETURN
      END
