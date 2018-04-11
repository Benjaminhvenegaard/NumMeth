      SUBROUTINE IMTQL1 (N, D, E, IERR)
C
      INTEGER I,J,L,M,N,II,MML,IERR
      REAL D(*),E(*)
      REAL B,C,F,G,P,R,S,S1,S2
      REAL PYTHAG
C
C***FIRST EXECUTABLE STATEMENT  IMTQL1
      IERR = 0
      IF (N .EQ. 1) GO TO 1001
C
      DO 100 I = 2, N
  100 E(I-1) = E(I)
C
      E(N) = 0.0E0
C
      DO 290 L = 1, N
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
         IF (M .EQ. L) GO TO 215
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
  200    CONTINUE
C
         D(L) = D(L) - P
         E(L) = G
         E(M) = 0.0E0
         GO TO 105
C     .......... ORDER EIGENVALUES ..........
  215    IF (L .EQ. 1) GO TO 250
C     .......... FOR I=L STEP -1 UNTIL 2 DO -- ..........
         DO 230 II = 2, L
            I = L + 2 - II
            IF (P .GE. D(I-1)) GO TO 270
            D(I) = D(I-1)
  230    CONTINUE
C
  250    I = 1
  270    D(I) = P
  290 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 IERR = L
 1001 RETURN
      END
