      SUBROUTINE IMTQL2 (NM, N, D, E, Z, IERR)
C
      INTEGER I,J,K,L,M,N,II,NM,MML,IERR
      REAL D(*),E(*),Z(NM,*)
      REAL B,C,F,G,P,R,S,S1,S2
      REAL PYTHAG
C
C***FIRST EXECUTABLE STATEMENT  IMTQL2
      IERR = 0
      IF (N .EQ. 1) GO TO 1001
C
      DO 100 I = 2, N
  100 E(I-1) = E(I)
C
      E(N) = 0.0E0
C
      DO 240 L = 1, N
         J = 0
C     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT ..........
  105    DO 110 M = L, N
            IF (M .EQ. N) GO TO 120
            S1 = ABS(D(M)) + ABS(D(M+1))
            S2 = S1 + ABS(E(M))
            IF (S2 .EQ. S1) GO TO 120
  110    CONTINUE
C
  120    P = D(L)
         IF (M .EQ. L) GO TO 240
         IF (J .EQ. 30) GO TO 1000
         J = J + 1
C     .......... FORM SHIFT ..........
         G = (D(L+1) - P) / (2.0E0 * E(L))
         R = PYTHAG(G,1.0E0)
         G = D(M) - P + E(L) / (G + SIGN(R,G))
         S = 1.0E0
         C = 1.0E0
         P = 0.0E0
         MML = M - L
C     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
         DO 200 II = 1, MML
            I = M - II
            F = S * E(I)
            B = C * E(I)
            IF (ABS(F) .LT. ABS(G)) GO TO 150
            C = G / F
            R = SQRT(C*C+1.0E0)
            E(I+1) = F * R
            S = 1.0E0 / R
            C = C * S
            GO TO 160
  150       S = F / G
            R = SQRT(S*S+1.0E0)
            E(I+1) = G * R
            C = 1.0E0 / R
            S = S * C
  160       G = D(I+1) - P
            R = (D(I) - G) * S + 2.0E0 * C * B
            P = S * R
            D(I+1) = G + P
            G = C * R - B
C     .......... FORM VECTOR ..........
            DO 180 K = 1, N
               F = Z(K,I+1)
               Z(K,I+1) = S * Z(K,I) + C * F
               Z(K,I) = C * Z(K,I) - S * F
  180       CONTINUE
C
  200    CONTINUE
C
         D(L) = D(L) - P
         E(L) = G
         E(M) = 0.0E0
         GO TO 105
  240 CONTINUE
C     .......... ORDER EIGENVALUES AND EIGENVECTORS ..........
      DO 300 II = 2, N
         I = II - 1
         K = I
         P = D(I)
C
         DO 260 J = II, N
            IF (D(J) .GE. P) GO TO 260
            K = J
            P = D(J)
  260    CONTINUE
C
         IF (K .EQ. I) GO TO 300
         D(K) = D(I)
         D(I) = P
C
         DO 280 J = 1, N
            P = Z(J,I)
            Z(J,I) = Z(J,K)
            Z(J,K) = P
  280    CONTINUE
C
  300 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 IERR = L
 1001 RETURN
      END
