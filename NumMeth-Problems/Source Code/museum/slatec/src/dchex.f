      SUBROUTINE DCHEX (R, LDR, P, K, L, Z, LDZ, NZ, C, S, JOB)
      INTEGER LDR,P,K,L,LDZ,NZ,JOB
      DOUBLE PRECISION R(LDR,*),Z(LDZ,*),S(*)
      DOUBLE PRECISION C(*)
C
      INTEGER I,II,IL,IU,J,JJ,KM1,KP1,LMK,LM1
      DOUBLE PRECISION T
C
C     INITIALIZE
C
C***FIRST EXECUTABLE STATEMENT  DCHEX
      KM1 = K - 1
      KP1 = K + 1
      LMK = L - K
      LM1 = L - 1
C
C     PERFORM THE APPROPRIATE TASK.
C
      GO TO (10,130), JOB
C
C     RIGHT CIRCULAR SHIFT.
C
   10 CONTINUE
C
C        REORDER THE COLUMNS.
C
         DO 20 I = 1, L
            II = L - I + 1
            S(I) = R(II,L)
   20    CONTINUE
         DO 40 JJ = K, LM1
            J = LM1 - JJ + K
            DO 30 I = 1, J
               R(I,J+1) = R(I,J)
   30       CONTINUE
            R(J+1,J+1) = 0.0D0
   40    CONTINUE
         IF (K .EQ. 1) GO TO 60
            DO 50 I = 1, KM1
               II = L - I + 1
               R(I,K) = S(II)
   50       CONTINUE
   60    CONTINUE
C
C        CALCULATE THE ROTATIONS.
C
         T = S(1)
         DO 70 I = 1, LMK
            CALL DROTG(S(I+1),T,C(I),S(I))
            T = S(I+1)
   70    CONTINUE
         R(K,K) = T
         DO 90 J = KP1, P
            IL = MAX(1,L-J+1)
            DO 80 II = IL, LMK
               I = L - II
               T = C(II)*R(I,J) + S(II)*R(I+1,J)
               R(I+1,J) = C(II)*R(I+1,J) - S(II)*R(I,J)
               R(I,J) = T
   80       CONTINUE
   90    CONTINUE
C
C        IF REQUIRED, APPLY THE TRANSFORMATIONS TO Z.
C
         IF (NZ .LT. 1) GO TO 120
         DO 110 J = 1, NZ
            DO 100 II = 1, LMK
               I = L - II
               T = C(II)*Z(I,J) + S(II)*Z(I+1,J)
               Z(I+1,J) = C(II)*Z(I+1,J) - S(II)*Z(I,J)
               Z(I,J) = T
  100       CONTINUE
  110    CONTINUE
  120    CONTINUE
      GO TO 260
C
C     LEFT CIRCULAR SHIFT
C
  130 CONTINUE
C
C        REORDER THE COLUMNS
C
         DO 140 I = 1, K
            II = LMK + I
            S(II) = R(I,K)
  140    CONTINUE
         DO 160 J = K, LM1
            DO 150 I = 1, J
               R(I,J) = R(I,J+1)
  150       CONTINUE
            JJ = J - KM1
            S(JJ) = R(J+1,J+1)
  160    CONTINUE
         DO 170 I = 1, K
            II = LMK + I
            R(I,L) = S(II)
  170    CONTINUE
         DO 180 I = KP1, L
            R(I,L) = 0.0D0
  180    CONTINUE
C
C        REDUCTION LOOP.
C
         DO 220 J = K, P
            IF (J .EQ. K) GO TO 200
C
C              APPLY THE ROTATIONS.
C
               IU = MIN(J-1,L-1)
               DO 190 I = K, IU
                  II = I - K + 1
                  T = C(II)*R(I,J) + S(II)*R(I+1,J)
                  R(I+1,J) = C(II)*R(I+1,J) - S(II)*R(I,J)
                  R(I,J) = T
  190          CONTINUE
  200       CONTINUE
            IF (J .GE. L) GO TO 210
               JJ = J - K + 1
               T = S(JJ)
               CALL DROTG(R(J,J),T,C(JJ),S(JJ))
  210       CONTINUE
  220    CONTINUE
C
C        APPLY THE ROTATIONS TO Z.
C
         IF (NZ .LT. 1) GO TO 250
         DO 240 J = 1, NZ
            DO 230 I = K, LM1
               II = I - KM1
               T = C(II)*Z(I,J) + S(II)*Z(I+1,J)
               Z(I+1,J) = C(II)*Z(I+1,J) - S(II)*Z(I,J)
               Z(I,J) = T
  230       CONTINUE
  240    CONTINUE
  250    CONTINUE
  260 CONTINUE
      RETURN
      END
