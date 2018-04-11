      SUBROUTINE CPOFA (A, LDA, N, INFO)
      INTEGER LDA,N,INFO
      COMPLEX A(LDA,*)
C
      COMPLEX CDOTC,T
      REAL S
      INTEGER J,JM1,K
C***FIRST EXECUTABLE STATEMENT  CPOFA
         DO 30 J = 1, N
            INFO = J
            S = 0.0E0
            JM1 = J - 1
            IF (JM1 .LT. 1) GO TO 20
            DO 10 K = 1, JM1
               T = A(K,J) - CDOTC(K-1,A(1,K),1,A(1,J),1)
               T = T/A(K,K)
               A(K,J) = T
               S = S + REAL(T*CONJG(T))
   10       CONTINUE
   20       CONTINUE
            S = REAL(A(J,J)) - S
            IF (S .LE. 0.0E0 .OR. AIMAG(A(J,J)) .NE. 0.0E0) GO TO 40
            A(J,J) = CMPLX(SQRT(S),0.0E0)
   30    CONTINUE
         INFO = 0
   40 CONTINUE
      RETURN
      END
