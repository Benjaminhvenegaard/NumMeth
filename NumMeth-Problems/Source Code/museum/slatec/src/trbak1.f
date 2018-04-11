      SUBROUTINE TRBAK1 (NM, N, A, E, M, Z)
C
      INTEGER I,J,K,L,M,N,NM
      REAL A(NM,*),E(*),Z(NM,*)
      REAL S
C
C***FIRST EXECUTABLE STATEMENT  TRBAK1
      IF (M .EQ. 0) GO TO 200
      IF (N .EQ. 1) GO TO 200
C
      DO 140 I = 2, N
         L = I - 1
         IF (E(I) .EQ. 0.0E0) GO TO 140
C
         DO 130 J = 1, M
            S = 0.0E0
C
            DO 110 K = 1, L
  110       S = S + A(I,K) * Z(K,J)
C     .......... DIVISOR BELOW IS NEGATIVE OF H FORMED IN TRED1.
C                DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW ..........
            S = (S / A(I,L)) / E(I)
C
            DO 120 K = 1, L
  120       Z(K,J) = Z(K,J) + S * A(I,K)
C
  130    CONTINUE
C
  140 CONTINUE
C
  200 RETURN
      END
