      SUBROUTINE IMTQLV (N, D, E, E2, W, IND, IERR, RV1)
C
      INTEGER I,J,K,L,M,N,II,MML,TAG,IERR
      REAL D(*),E(*),E2(*),W(*),RV1(*)
      REAL B,C,F,G,P,R,S,S1,S2
      REAL PYTHAG
      INTEGER IND(*)
C
C***FIRST EXECUTABLE STATEMENT  IMTQLV
      IERR = 0
      K = 0
      TAG = 0
C
      DO 100 I = 1, N
         W(I) = D(I)
         IF (I .NE. 1) RV1(I-1) = E(I)
  100 CONTINUE
C
      E2(1) = 0.0E0
      RV1(N) = 0.0E0
C
      DO 290 L = 1, N
         J = 0
C     .......... LOOK FOR SMALL SUB-DIAGONAL ELEMENT ..........
  105    DO 110 M = L, N
            IF (M .EQ. N) GO TO 120
            S1 = ABS(W(M)) + ABS(W(M+1))
            S2 = S1 + ABS(RV1(M))
            IF (S2 .EQ. S1) GO TO 120
C     .......... GUARD AGAINST UNDERFLOWED ELEMENT OF E2 ..........
            IF (E2(M+1) .EQ. 0.0E0) GO TO 125
  110    CONTINUE
C
  120    IF (M .LE. K) GO TO 130
         IF (M .NE. N) E2(M+1) = 0.0E0
  125    K = M
         TAG = TAG + 1
  130    P = W(L)
         IF (M .EQ. L) GO TO 215
         IF (J .EQ. 30) GO TO 1000
         J = J + 1
C     .......... FORM SHIFT ..........
         G = (W(L+1) - P) / (2.0E0 * RV1(L))
         R = PYTHAG(G,1.0E0)
         G = W(M) - P + RV1(L) / (G + SIGN(R,G))
         S = 1.0E0
         C = 1.0E0
         P = 0.0E0
         MML = M - L
C     .......... FOR I=M-1 STEP -1 UNTIL L DO -- ..........
         DO 200 II = 1, MML
            I = M - II
            F = S * RV1(I)
            B = C * RV1(I)
            IF (ABS(F) .LT. ABS(G)) GO TO 150
            C = G / F
            R = SQRT(C*C+1.0E0)
            RV1(I+1) = F * R
            S = 1.0E0 / R
            C = C * S
            GO TO 160
  150       S = F / G
            R = SQRT(S*S+1.0E0)
            RV1(I+1) = G * R
            C = 1.0E0 / R
            S = S * C
  160       G = W(I+1) - P
            R = (W(I) - G) * S + 2.0E0 * C * B
            P = S * R
            W(I+1) = G + P
            G = C * R - B
  200    CONTINUE
C
         W(L) = W(L) - P
         RV1(L) = G
         RV1(M) = 0.0E0
         GO TO 105
C     .......... ORDER EIGENVALUES ..........
  215    IF (L .EQ. 1) GO TO 250
C     .......... FOR I=L STEP -1 UNTIL 2 DO -- ..........
         DO 230 II = 2, L
            I = L + 2 - II
            IF (P .GE. W(I-1)) GO TO 270
            W(I) = W(I-1)
            IND(I) = IND(I-1)
  230    CONTINUE
C
  250    I = 1
  270    W(I) = P
         IND(I) = TAG
  290 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- NO CONVERGENCE TO AN
C                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 IERR = L
 1001 RETURN
      END
