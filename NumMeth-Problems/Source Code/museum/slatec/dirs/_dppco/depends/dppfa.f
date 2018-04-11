      SUBROUTINE DPPFA (AP, N, INFO)
      INTEGER N,INFO
      DOUBLE PRECISION AP(*)
C
      DOUBLE PRECISION DDOT,T
      DOUBLE PRECISION S
      INTEGER J,JJ,JM1,K,KJ,KK
C***FIRST EXECUTABLE STATEMENT  DPPFA
         JJ = 0
         DO 30 J = 1, N
            INFO = J
            S = 0.0D0
            JM1 = J - 1
            KJ = JJ
            KK = 0
            IF (JM1 .LT. 1) GO TO 20
            DO 10 K = 1, JM1
               KJ = KJ + 1
               T = AP(KJ) - DDOT(K-1,AP(KK+1),1,AP(JJ+1),1)
               KK = KK + K
               T = T/AP(KK)
               AP(KJ) = T
               S = S + T*T
   10       CONTINUE
   20       CONTINUE
            JJ = JJ + J
            S = AP(JJ) - S
            IF (S .LE. 0.0D0) GO TO 40
            AP(JJ) = SQRT(S)
   30    CONTINUE
         INFO = 0
   40 CONTINUE
      RETURN
      END
