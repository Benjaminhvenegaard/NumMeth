      SUBROUTINE RATQR (N, EPS1, D, E, E2, M, W, IND, BD, TYPE, IDEF,
     +   IERR)
C
      INTEGER I,J,K,M,N,II,JJ,K1,IDEF,IERR,JDEF
      REAL D(*),E(*),E2(*),W(*),BD(*)
      REAL F,P,Q,R,S,EP,QP,ERR,TOT,EPS1,DELTA,MACHEP
      INTEGER IND(*)
      LOGICAL FIRST, TYPE
C
      SAVE FIRST, MACHEP
      DATA FIRST /.TRUE./
C***FIRST EXECUTABLE STATEMENT  RATQR
      IF (FIRST) THEN
         MACHEP = R1MACH(4)
      ENDIF
      FIRST = .FALSE.
C
      IERR = 0
      JDEF = IDEF
C     .......... COPY D ARRAY INTO W ..........
      DO 20 I = 1, N
   20 W(I) = D(I)
C
      IF (TYPE) GO TO 40
      J = 1
      GO TO 400
   40 ERR = 0.0E0
      S = 0.0E0
C     .......... LOOK FOR SMALL SUB-DIAGONAL ENTRIES AND DEFINE
C                INITIAL SHIFT FROM LOWER GERSCHGORIN BOUND.
C                COPY E2 ARRAY INTO BD ..........
      TOT = W(1)
      Q = 0.0E0
      J = 0
C
      DO 100 I = 1, N
         P = Q
         IF (I .EQ. 1) GO TO 60
         IF (P .GT. MACHEP * (ABS(D(I)) + ABS(D(I-1)))) GO TO 80
   60    E2(I) = 0.0E0
   80    BD(I) = E2(I)
C     .......... COUNT ALSO IF ELEMENT OF E2 HAS UNDERFLOWED ..........
         IF (E2(I) .EQ. 0.0E0) J = J + 1
         IND(I) = J
         Q = 0.0E0
         IF (I .NE. N) Q = ABS(E(I+1))
         TOT = MIN(W(I)-P-Q,TOT)
  100 CONTINUE
C
      IF (JDEF .EQ. 1 .AND. TOT .LT. 0.0E0) GO TO 140
C
      DO 110 I = 1, N
  110 W(I) = W(I) - TOT
C
      GO TO 160
  140 TOT = 0.0E0
C
  160 DO 360 K = 1, M
C     .......... NEXT QR TRANSFORMATION ..........
  180    TOT = TOT + S
         DELTA = W(N) - S
         I = N
         F = ABS(MACHEP*TOT)
         IF (EPS1 .LT. F) EPS1 = F
         IF (DELTA .GT. EPS1) GO TO 190
         IF (DELTA .LT. (-EPS1)) GO TO 1000
         GO TO 300
C     .......... REPLACE SMALL SUB-DIAGONAL SQUARES BY ZERO
C                TO REDUCE THE INCIDENCE OF UNDERFLOWS ..........
  190    IF (K .EQ. N) GO TO 210
         K1 = K + 1
         DO 200 J = K1, N
            IF (BD(J) .LE. (MACHEP*(W(J)+W(J-1))) ** 2) BD(J) = 0.0E0
  200    CONTINUE
C
  210    F = BD(N) / DELTA
         QP = DELTA + F
         P = 1.0E0
         IF (K .EQ. N) GO TO 260
         K1 = N - K
C     .......... FOR I=N-1 STEP -1 UNTIL K DO -- ..........
         DO 240 II = 1, K1
            I = N - II
            Q = W(I) - S - F
            R = Q / QP
            P = P * R + 1.0E0
            EP = F * R
            W(I+1) = QP + EP
            DELTA = Q - EP
            IF (DELTA .GT. EPS1) GO TO 220
            IF (DELTA .LT. (-EPS1)) GO TO 1000
            GO TO 300
  220       F = BD(I) / Q
            QP = DELTA + F
            BD(I+1) = QP * EP
  240    CONTINUE
C
  260    W(K) = QP
         S = QP / P
         IF (TOT + S .GT. TOT) GO TO 180
C     .......... SET ERROR -- IRREGULAR END OF ITERATION.
C                DEFLATE MINIMUM DIAGONAL ELEMENT ..........
         IERR = 5 * N + K
         S = 0.0E0
         DELTA = QP
C
         DO 280 J = K, N
            IF (W(J) .GT. DELTA) GO TO 280
            I = J
            DELTA = W(J)
  280    CONTINUE
C     .......... CONVERGENCE ..........
  300    IF (I .LT. N) BD(I+1) = BD(I) * F / QP
         II = IND(I)
         IF (I .EQ. K) GO TO 340
         K1 = I - K
C     .......... FOR J=I-1 STEP -1 UNTIL K DO -- ..........
         DO 320 JJ = 1, K1
            J = I - JJ
            W(J+1) = W(J) - S
            BD(J+1) = BD(J)
            IND(J+1) = IND(J)
  320    CONTINUE
C
  340    W(K) = TOT
         ERR = ERR + ABS(DELTA)
         BD(K) = ERR
         IND(K) = II
  360 CONTINUE
C
      IF (TYPE) GO TO 1001
      F = BD(1)
      E2(1) = 2.0E0
      BD(1) = F
      J = 2
C     .......... NEGATE ELEMENTS OF W FOR LARGEST VALUES ..........
  400 DO 500 I = 1, N
  500 W(I) = -W(I)
C
      JDEF = -JDEF
      GO TO (40,1001), J
C     .......... SET ERROR -- IDEF SPECIFIED INCORRECTLY ..........
 1000 IERR = 6 * N + 1
 1001 RETURN
      END
