      SUBROUTINE TRBAK3 (NM, N, NV, A, M, Z)
C
      INTEGER I,J,K,L,M,N,IK,IZ,NM,NV
      REAL A(*),Z(NM,*)
      REAL H,S
C
C***FIRST EXECUTABLE STATEMENT  TRBAK3
      IF (M .EQ. 0) GO TO 200
      IF (N .EQ. 1) GO TO 200
C
      DO 140 I = 2, N
         L = I - 1
         IZ = (I * L) / 2
         IK = IZ + I
         H = A(IK)
         IF (H .EQ. 0.0E0) GO TO 140
C
         DO 130 J = 1, M
            S = 0.0E0
            IK = IZ
C
            DO 110 K = 1, L
               IK = IK + 1
               S = S + A(IK) * Z(K,J)
  110       CONTINUE
C     .......... DOUBLE DIVISION AVOIDS POSSIBLE UNDERFLOW ..........
            S = (S / H) / H
            IK = IZ
C
            DO 120 K = 1, L
               IK = IK + 1
               Z(K,J) = Z(K,J) - S * A(IK)
  120       CONTINUE
C
  130    CONTINUE
C
  140 CONTINUE
C
  200 RETURN
      END
