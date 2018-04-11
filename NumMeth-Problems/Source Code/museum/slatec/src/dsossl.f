      SUBROUTINE DSOSSL (K, N, L, X, C, B, M)
C
C
      INTEGER J, JKM, K, KJ, KM, KM1, KMM1, KN, L, LK, M, N, NP1
      DOUBLE PRECISION B(*), C(*), X(*), XMAX
C
C***FIRST EXECUTABLE STATEMENT  DSOSSL
      NP1 = N + 1
      KM1 = K - 1
      LK = KM1
      IF (L .EQ. K) LK = K
      KN = M
C
C
      DO 40 KJ = 1, KM1
         KMM1 = K - KJ
         KM = KMM1 + 1
         XMAX = 0.0D0
         KN = KN - NP1 + KMM1
         IF (KM .GT. LK) GO TO 20
            JKM = KN
C
            DO 10 J = KM, LK
               JKM = JKM + 1
               XMAX = XMAX + C(JKM)*X(J)
   10       CONTINUE
   20    CONTINUE
C
         IF (L .LE. K) GO TO 30
            JKM = KN + L - KMM1
            XMAX = XMAX + C(JKM)*X(L)
   30    CONTINUE
         X(KMM1) = XMAX + B(KMM1)
   40 CONTINUE
C
      RETURN
      END
