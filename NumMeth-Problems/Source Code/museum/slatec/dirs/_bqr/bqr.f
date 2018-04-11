      SUBROUTINE BQR (NM, N, MB, A, T, R, IERR, NV, RV)
C
      INTEGER I,J,K,L,M,N,II,IK,JK,JM,KJ,KK,KM,LL,MB,MK,MN,MZ
      INTEGER M1,M2,M3,M4,NI,NM,NV,ITS,KJ1,M21,M31,IERR,IMULT
      REAL A(NM,*),RV(*)
      REAL F,G,Q,R,S,T,SCALE
      REAL PYTHAG
C
C***FIRST EXECUTABLE STATEMENT  BQR
      IERR = 0
      M1 = MIN(MB,N)
      M = M1 - 1
      M2 = M + M
      M21 = M2 + 1
      M3 = M21 + M
      M31 = M3 + 1
      M4 = M31 + M2
      MN = M + N
      MZ = MB - M1
      ITS = 0
C     .......... TEST FOR CONVERGENCE ..........
   40 G = A(N,MB)
      IF (M .EQ. 0) GO TO 360
      F = 0.0E0
C
      DO 50 K = 1, M
         MK = K + MZ
         F = F + ABS(A(N,MK))
   50 CONTINUE
C
      IF (ITS .EQ. 0 .AND. F .GT. R) R = F
      IF (R + F .LE. R) GO TO 360
      IF (ITS .EQ. 30) GO TO 1000
      ITS = ITS + 1
C     .......... FORM SHIFT FROM BOTTOM 2 BY 2 MINOR ..........
      IF (F .GT. 0.25E0 * R .AND. ITS .LT. 5) GO TO 90
      F = A(N,MB-1)
      IF (F .EQ. 0.0E0) GO TO 70
      Q = (A(N-1,MB) - G) / (2.0E0 * F)
      S = PYTHAG(Q,1.0E0)
      G = G - F / (Q + SIGN(S,Q))
   70 T = T + G
C
      DO 80 I = 1, N
   80 A(I,MB) = A(I,MB) - G
C
   90 DO 100 K = M31, M4
  100 RV(K) = 0.0E0
C
      DO 350 II = 1, MN
         I = II - M
         NI = N - II
         IF (NI .LT. 0) GO TO 230
C     .......... FORM COLUMN OF SHIFTED MATRIX A-G*I ..........
         L = MAX(1,2-I)
C
         DO 110 K = 1, M3
  110    RV(K) = 0.0E0
C
         DO 120 K = L, M1
            KM = K + M
            MK = K + MZ
            RV(KM) = A(II,MK)
  120    CONTINUE
C
         LL = MIN(M,NI)
         IF (LL .EQ. 0) GO TO 135
C
         DO 130 K = 1, LL
            KM = K + M21
            IK = II + K
            MK = MB - K
            RV(KM) = A(IK,MK)
  130    CONTINUE
C     .......... PRE-MULTIPLY WITH HOUSEHOLDER REFLECTIONS ..........
  135    LL = M2
         IMULT = 0
C     .......... MULTIPLICATION PROCEDURE ..........
  140    KJ = M4 - M1
C
         DO 170 J = 1, LL
            KJ = KJ + M1
            JM = J + M3
            IF (RV(JM) .EQ. 0.0E0) GO TO 170
            F = 0.0E0
C
            DO 150 K = 1, M1
               KJ = KJ + 1
               JK = J + K - 1
               F = F + RV(KJ) * RV(JK)
  150       CONTINUE
C
            F = F / RV(JM)
            KJ = KJ - M1
C
            DO 160 K = 1, M1
               KJ = KJ + 1
               JK = J + K - 1
               RV(JK) = RV(JK) - RV(KJ) * F
  160       CONTINUE
C
            KJ = KJ - M1
  170    CONTINUE
C
         IF (IMULT .NE. 0) GO TO 280
C     .......... HOUSEHOLDER REFLECTION ..........
         F = RV(M21)
         S = 0.0E0
         RV(M4) = 0.0E0
         SCALE = 0.0E0
C
         DO 180 K = M21, M3
  180    SCALE = SCALE + ABS(RV(K))
C
         IF (SCALE .EQ. 0.0E0) GO TO 210
C
         DO 190 K = M21, M3
  190    S = S + (RV(K)/SCALE)**2
C
         S = SCALE * SCALE * S
         G = -SIGN(SQRT(S),F)
         RV(M21) = G
         RV(M4) = S - F * G
         KJ = M4 + M2 * M1 + 1
         RV(KJ) = F - G
C
         DO 200 K = 2, M1
            KJ = KJ + 1
            KM = K + M2
            RV(KJ) = RV(KM)
  200    CONTINUE
C     .......... SAVE COLUMN OF TRIANGULAR FACTOR R ..........
  210    DO 220 K = L, M1
            KM = K + M
            MK = K + MZ
            A(II,MK) = RV(KM)
  220    CONTINUE
C
  230    L = MAX(1,M1+1-I)
         IF (I .LE. 0) GO TO 300
C     .......... PERFORM ADDITIONAL STEPS ..........
         DO 240 K = 1, M21
  240    RV(K) = 0.0E0
C
         LL = MIN(M1,NI+M1)
C     .......... GET ROW OF TRIANGULAR FACTOR R ..........
         DO 250 KK = 1, LL
            K = KK - 1
            KM = K + M1
            IK = I + K
            MK = MB - K
            RV(KM) = A(IK,MK)
  250    CONTINUE
C     .......... POST-MULTIPLY WITH HOUSEHOLDER REFLECTIONS ..........
         LL = M1
         IMULT = 1
         GO TO 140
C     .......... STORE COLUMN OF NEW A MATRIX ..........
  280    DO 290 K = L, M1
            MK = K + MZ
            A(I,MK) = RV(K)
  290    CONTINUE
C     .......... UPDATE HOUSEHOLDER REFLECTIONS ..........
  300    IF (L .GT. 1) L = L - 1
         KJ1 = M4 + L * M1
C
         DO 320 J = L, M2
            JM = J + M3
            RV(JM) = RV(JM+1)
C
            DO 320 K = 1, M1
               KJ1 = KJ1 + 1
               KJ = KJ1 - M1
               RV(KJ) = RV(KJ1)
  320    CONTINUE
C
  350 CONTINUE
C
      GO TO 40
C     .......... CONVERGENCE ..........
  360 T = T + G
C
      DO 380 I = 1, N
  380 A(I,MB) = A(I,MB) - G
C
      DO 400 K = 1, M1
         MK = K + MZ
         A(N,MK) = 0.0E0
  400 CONTINUE
C
      GO TO 1001
C     .......... SET ERROR -- NO CONVERGENCE TO
C                EIGENVALUE AFTER 30 ITERATIONS ..........
 1000 IERR = N
 1001 RETURN
      END
