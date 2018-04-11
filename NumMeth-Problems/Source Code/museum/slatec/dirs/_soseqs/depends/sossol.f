      SUBROUTINE SOSSOL (K, N, L, X, C, B, M)
C
C
      DIMENSION X(*), C(*), B(*)
C
C***FIRST EXECUTABLE STATEMENT  SOSSOL
      NP1 = N + 1
      KM1 = K - 1
      LK = KM1
      IF (L .EQ. K) LK = K
      KN = M
C
C
      DO 40 KJ=1,KM1
        KMM1 = K - KJ
        KM = KMM1 + 1
        XMAX = 0.
        KN = KN - NP1 + KMM1
        IF (KM .GT. LK) GO TO 20
        JKM = KN
C
        DO 10 J=KM,LK
          JKM = JKM + 1
          XMAX = XMAX + C(JKM)*X(J)
   10   CONTINUE
C
   20   IF (L .LE. K) GO TO 30
        JKM = KN + L - KMM1
        XMAX = XMAX + C(JKM)*X(L)
   30   X(KMM1) = XMAX + B(KMM1)
   40 CONTINUE
C
      RETURN
      END
