      SUBROUTINE SHELS (A, LDA, N, Q, B)
C         The following is for optimized compilation on LLNL/LTSS Crays.
CLLL. OPTIMIZE
C     .. Scalar Arguments ..
      INTEGER LDA, N
C     .. Array Arguments ..
      REAL A(LDA,*), B(*), Q(*)
C     .. Local Scalars ..
      REAL C, S, T, T1, T2
      INTEGER IQ, K, KB, KP1
C     .. External Subroutines ..
      EXTERNAL SAXPY
C***FIRST EXECUTABLE STATEMENT  SHELS
C
C         Minimize(B-A*X,B-A*X).  First form Q*B.
C
      DO 20 K = 1, N
         KP1 = K + 1
         IQ = 2*(K-1) + 1
         C = Q(IQ)
         S = Q(IQ+1)
         T1 = B(K)
         T2 = B(KP1)
         B(K) = C*T1 - S*T2
         B(KP1) = S*T1 + C*T2
 20   CONTINUE
C
C         Now solve  R*X = Q*B.
C
      DO 40 KB = 1, N
         K = N + 1 - KB
         B(K) = B(K)/A(K,K)
         T = -B(K)
         CALL SAXPY(K-1, T, A(1,K), 1, B(1), 1)
 40   CONTINUE
      RETURN
C------------- LAST LINE OF SHELS FOLLOWS ----------------------------
      END
