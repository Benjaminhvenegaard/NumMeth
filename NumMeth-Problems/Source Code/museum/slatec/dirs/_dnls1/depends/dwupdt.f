      SUBROUTINE DWUPDT (N, R, LDR, W, B, ALPHA, COS, SIN)
      INTEGER N,LDR
      DOUBLE PRECISION ALPHA
      DOUBLE PRECISION R(LDR,*),W(*),B(*),COS(*),SIN(*)
      INTEGER I,J,JM1
      DOUBLE PRECISION COTAN,ONE,P5,P25,ROWJ,TAN,TEMP,ZERO
      SAVE ONE, P5, P25, ZERO
      DATA ONE,P5,P25,ZERO /1.0D0,5.0D-1,2.5D-1,0.0D0/
C***FIRST EXECUTABLE STATEMENT  DWUPDT
      DO 60 J = 1, N
         ROWJ = W(J)
         JM1 = J - 1
C
C        APPLY THE PREVIOUS TRANSFORMATIONS TO
C        R(I,J), I=1,2,...,J-1, AND TO W(J).
C
         IF (JM1 .LT. 1) GO TO 20
         DO 10 I = 1, JM1
            TEMP = COS(I)*R(I,J) + SIN(I)*ROWJ
            ROWJ = -SIN(I)*R(I,J) + COS(I)*ROWJ
            R(I,J) = TEMP
   10       CONTINUE
   20    CONTINUE
C
C        DETERMINE A GIVENS ROTATION WHICH ELIMINATES W(J).
C
         COS(J) = ONE
         SIN(J) = ZERO
         IF (ROWJ .EQ. ZERO) GO TO 50
         IF (ABS(R(J,J)) .GE. ABS(ROWJ)) GO TO 30
            COTAN = R(J,J)/ROWJ
            SIN(J) = P5/SQRT(P25+P25*COTAN**2)
            COS(J) = SIN(J)*COTAN
            GO TO 40
   30    CONTINUE
            TAN = ROWJ/R(J,J)
            COS(J) = P5/SQRT(P25+P25*TAN**2)
            SIN(J) = COS(J)*TAN
   40    CONTINUE
C
C        APPLY THE CURRENT TRANSFORMATION TO R(J,J), B(J), AND ALPHA.
C
         R(J,J) = COS(J)*R(J,J) + SIN(J)*ROWJ
         TEMP = COS(J)*B(J) + SIN(J)*ALPHA
         ALPHA = -SIN(J)*B(J) + COS(J)*ALPHA
         B(J) = TEMP
   50    CONTINUE
   60    CONTINUE
      RETURN
C
C     LAST CARD OF SUBROUTINE DWUPDT.
C
      END
