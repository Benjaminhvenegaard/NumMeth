      SUBROUTINE DINTYD (T, K, YH, NYH, DKY, IFLAG)
C
      INTEGER I, IC, IER, IFLAG, IOWND, IOWNS, J, JB, JB2, JJ, JJ1,
     1      JP1, JSTART, K, KFLAG, L, MAXORD, METH, MITER, N, NFE,
     2      NJE, NQ, NQU, NST, NYH
      DOUBLE PRECISION C, DKY, EL0, H, HMIN, HMXI, HU, R, ROWND,
     1      ROWNS, S, T, TN, TP, UROUND, YH
      DIMENSION YH(NYH,*),DKY(*)
      COMMON /DDEBD1/ ROWND,ROWNS(210),EL0,H,HMIN,HMXI,HU,TN,UROUND,
     1                IOWND(14),IOWNS(6),IER,JSTART,KFLAG,L,METH,MITER,
     2                MAXORD,N,NQ,NST,NFE,NJE,NQU
C
C     BEGIN BLOCK PERMITTING ...EXITS TO 130
C***FIRST EXECUTABLE STATEMENT  DINTYD
         IFLAG = 0
         IF (K .LT. 0 .OR. K .GT. NQ) GO TO 110
            TP = TN - HU*(1.0D0 + 100.0D0*UROUND)
            IF ((T - TP)*(T - TN) .LE. 0.0D0) GO TO 10
               IFLAG = -2
C     .........EXIT
               GO TO 130
   10       CONTINUE
C
            S = (T - TN)/H
            IC = 1
            IF (K .EQ. 0) GO TO 30
               JJ1 = L - K
               DO 20 JJ = JJ1, NQ
                  IC = IC*JJ
   20          CONTINUE
   30       CONTINUE
            C = IC
            DO 40 I = 1, N
               DKY(I) = C*YH(I,L)
   40       CONTINUE
            IF (K .EQ. NQ) GO TO 90
               JB2 = NQ - K
               DO 80 JB = 1, JB2
                  J = NQ - JB
                  JP1 = J + 1
                  IC = 1
                  IF (K .EQ. 0) GO TO 60
                     JJ1 = JP1 - K
                     DO 50 JJ = JJ1, J
                        IC = IC*JJ
   50                CONTINUE
   60             CONTINUE
                  C = IC
                  DO 70 I = 1, N
                     DKY(I) = C*YH(I,JP1) + S*DKY(I)
   70             CONTINUE
   80          CONTINUE
C     .........EXIT
               IF (K .EQ. 0) GO TO 130
   90       CONTINUE
            R = H**(-K)
            DO 100 I = 1, N
               DKY(I) = R*DKY(I)
  100       CONTINUE
         GO TO 120
  110    CONTINUE
C
            IFLAG = -1
  120    CONTINUE
  130 CONTINUE
      RETURN
C     ----------------------- END OF SUBROUTINE DINTYD
C     -----------------------
      END
