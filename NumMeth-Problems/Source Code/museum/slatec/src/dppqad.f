      SUBROUTINE DPPQAD (LDC, C, XI, LXI, K, X1, X2, PQUAD)
C
      INTEGER I, II, IL, ILO, IL1, IL2, IM, K, LDC, LEFT, LXI, MF1, MF2
      DOUBLE PRECISION A,AA,BB,C,DX,FLK,PQUAD,Q,S,SS,TA,TB,X,XI,X1,X2
      DIMENSION XI(*), C(LDC,*), SS(2)
C
C***FIRST EXECUTABLE STATEMENT  DPPQAD
      PQUAD = 0.0D0
      IF(K.LT.1) GO TO 100
      IF(LXI.LT.1) GO TO 105
      IF(LDC.LT.K) GO TO 110
      AA = MIN(X1,X2)
      BB = MAX(X1,X2)
      IF (AA.EQ.BB) RETURN
      ILO = 1
      CALL DINTRV(XI, LXI, AA, ILO, IL1, MF1)
      CALL DINTRV(XI, LXI, BB, ILO, IL2, MF2)
      Q = 0.0D0
      DO 40 LEFT=IL1,IL2
        TA = XI(LEFT)
        A = MAX(AA,TA)
        IF (LEFT.EQ.1) A = AA
        TB = BB
        IF (LEFT.LT.LXI) TB = XI(LEFT+1)
        X = MIN(BB,TB)
        DO 30 II=1,2
          SS(II) = 0.0D0
          DX = X - XI(LEFT)
          IF (DX.EQ.0.0D0) GO TO 20
          S = C(K,LEFT)
          FLK = K
          IM = K - 1
          IL = IM
          DO 10 I=1,IL
            S = S*DX/FLK + C(IM,LEFT)
            IM = IM - 1
            FLK = FLK - 1.0D0
   10     CONTINUE
          SS(II) = S*DX
   20     CONTINUE
          X = A
   30   CONTINUE
        Q = Q + (SS(1)-SS(2))
   40 CONTINUE
      IF (X1.GT.X2) Q = -Q
      PQUAD = Q
      RETURN
C
C
  100 CONTINUE
      CALL XERMSG ('SLATEC', 'DPPQAD', 'K DOES NOT SATISFY K.GE.1', 2,
     +   1)
      RETURN
  105 CONTINUE
      CALL XERMSG ('SLATEC', 'DPPQAD', 'LXI DOES NOT SATISFY LXI.GE.1',
     +   2, 1)
      RETURN
  110 CONTINUE
      CALL XERMSG ('SLATEC', 'DPPQAD', 'LDC DOES NOT SATISFY LDC.GE.K',
     +   2, 1)
      RETURN
      END
