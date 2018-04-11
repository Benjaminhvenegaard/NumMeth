      SUBROUTINE CPPSL (AP, N, B)
      INTEGER N
      COMPLEX AP(*),B(*)
C
      COMPLEX CDOTC,T
      INTEGER K,KB,KK
C***FIRST EXECUTABLE STATEMENT  CPPSL
      KK = 0
      DO 10 K = 1, N
         T = CDOTC(K-1,AP(KK+1),1,B(1),1)
         KK = KK + K
         B(K) = (B(K) - T)/AP(KK)
   10 CONTINUE
      DO 20 KB = 1, N
         K = N + 1 - KB
         B(K) = B(K)/AP(KK)
         KK = KK - K
         T = -B(K)
         CALL CAXPY(K-1,T,AP(KK+1),1,B(1),1)
   20 CONTINUE
      RETURN
      END
