      SUBROUTINE CPOSL (A, LDA, N, B)
      INTEGER LDA,N
      COMPLEX A(LDA,*),B(*)
C
      COMPLEX CDOTC,T
      INTEGER K,KB
C
C     SOLVE CTRANS(R)*Y = B
C
C***FIRST EXECUTABLE STATEMENT  CPOSL
      DO 10 K = 1, N
         T = CDOTC(K-1,A(1,K),1,B(1),1)
         B(K) = (B(K) - T)/A(K,K)
   10 CONTINUE
C
C     SOLVE R*X = Y
C
      DO 20 KB = 1, N
         K = N + 1 - KB
         B(K) = B(K)/A(K,K)
         T = -B(K)
         CALL CAXPY(K-1,T,A(1,K),1,B(1),1)
   20 CONTINUE
      RETURN
      END
