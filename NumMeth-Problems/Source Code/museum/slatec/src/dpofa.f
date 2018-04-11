      SUBROUTINE DPOFA (A, LDA, N, INFO)
      INTEGER LDA,N,INFO
      DOUBLE PRECISION A(LDA,*)
C
      DOUBLE PRECISION DDOT,T
      DOUBLE PRECISION S
      INTEGER J,JM1,K
C***FIRST EXECUTABLE STATEMENT  DPOFA
         DO 30 J = 1, N
            INFO = J
            S = 0.0D0
            JM1 = J - 1
            IF (JM1 .LT. 1) GO TO 20
            DO 10 K = 1, JM1
               T = A(K,J) - DDOT(K-1,A(1,K),1,A(1,J),1)
               T = T/A(K,K)
               A(K,J) = T
               S = S + T*T
   10       CONTINUE
   20       CONTINUE
            S = A(J,J) - S
            IF (S .LE. 0.0D0) GO TO 40
            A(J,J) = SQRT(S)
   30    CONTINUE
         INFO = 0
   40 CONTINUE
      RETURN
      END
