      SUBROUTINE INTYD (T, K, YH, NYH, DKY, IFLAG)
C
CLLL. OPTIMIZE
      INTEGER K, NYH, IFLAG, I, IC, IER, IOWND, IOWNS, J, JB, JB2,
     1   JJ, JJ1, JP1, JSTART, KFLAG, L, MAXORD, METH, MITER, N, NFE,
     2   NJE, NQ, NQU, NST
      REAL T, YH, DKY,
     1   ROWND, ROWNS, EL0, H, HMIN, HMXI, HU, TN, UROUND,
     2   C, R, S, TP
      DIMENSION YH(NYH,*), DKY(*)
      COMMON /DEBDF1/ ROWND, ROWNS(210),
     1   EL0, H, HMIN, HMXI, HU, TN, UROUND, IOWND(14), IOWNS(6),
     2   IER, JSTART, KFLAG, L, METH, MITER, MAXORD, N, NQ, NST, NFE,
     3   NJE, NQU
C
C***FIRST EXECUTABLE STATEMENT  INTYD
      IFLAG = 0
      IF (K .LT. 0 .OR. K .GT. NQ) GO TO 80
      TP = TN - HU*(1.0E0 + 100.0E0*UROUND)
      IF ((T-TP)*(T-TN) .GT. 0.0E0) GO TO 90
C
      S = (T - TN)/H
      IC = 1
      IF (K .EQ. 0) GO TO 15
      JJ1 = L - K
      DO 10 JJ = JJ1,NQ
 10     IC = IC*JJ
 15   C = IC
      DO 20 I = 1,N
 20     DKY(I) = C*YH(I,L)
      IF (K .EQ. NQ) GO TO 55
      JB2 = NQ - K
      DO 50 JB = 1,JB2
        J = NQ - JB
        JP1 = J + 1
        IC = 1
        IF (K .EQ. 0) GO TO 35
        JJ1 = JP1 - K
        DO 30 JJ = JJ1,J
 30       IC = IC*JJ
 35     C = IC
        DO 40 I = 1,N
 40       DKY(I) = C*YH(I,JP1) + S*DKY(I)
 50     CONTINUE
      IF (K .EQ. 0) RETURN
 55   R = H**(-K)
      DO 60 I = 1,N
 60     DKY(I) = R*DKY(I)
      RETURN
C
 80   IFLAG = -1
      RETURN
 90   IFLAG = -2
      RETURN
C----------------------- END OF SUBROUTINE INTYD -----------------------
      END
