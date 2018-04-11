      SUBROUTINE AVINT (X, Y, N, XLO, XUP, ANS, IERR)
C
      DOUBLE PRECISION R3,RP5,SUM,SYL,SYL2,SYL3,SYU,SYU2,SYU3,X1,X2,X3
     1,X12,X13,X23,TERM1,TERM2,TERM3,A,B,C,CA,CB,CC
      DIMENSION X(*),Y(*)
C***FIRST EXECUTABLE STATEMENT  AVINT
      IERR=1
      ANS =0.0
      IF (XLO-XUP) 3,100,200
    3 IF (N.LT.2) GO TO 215
      DO 5 I=2,N
      IF (X(I).LE.X(I-1)) GO TO 210
      IF (X(I).GT.XUP) GO TO 6
    5 CONTINUE
    6 CONTINUE
      IF (N.GE.3) GO TO 9
C
C     SPECIAL N=2 CASE
      SLOPE = (Y(2)-Y(1))/(X(2)-X(1))
      FL = Y(1) + SLOPE*(XLO-X(1))
      FR = Y(2) + SLOPE*(XUP-X(2))
      ANS = 0.5*(FL+FR)*(XUP-XLO)
      RETURN
    9 CONTINUE
      IF (X(N-2).LT.XLO)  GO TO 205
      IF (X(3).GT.XUP)    GO TO 205
      I = 1
   10 IF (X(I).GE.XLO) GO TO 15
      I = I+1
      GO TO 10
   15 INLFT = I
      I = N
   20 IF (X(I).LE.XUP) GO TO 25
      I = I-1
      GO TO 20
   25 INRT = I
      IF ((INRT-INLFT).LT.2) GO TO 205
      ISTART = INLFT
      IF (INLFT.EQ.1) ISTART = 2
      ISTOP  = INRT
      IF (INRT.EQ.N)  ISTOP  = N-1
C
      R3 = 3.0D0
      RP5= 0.5D0
      SUM = 0.0
      SYL = XLO
      SYL2= SYL*SYL
      SYL3= SYL2*SYL
C
      DO 50 I=ISTART,ISTOP
      X1 = X(I-1)
      X2 = X(I)
      X3 = X(I+1)
      X12 = X1-X2
      X13 = X1-X3
      X23 = X2-X3
      TERM1 = DBLE(Y(I-1))/(X12*X13)
      TERM2 =-DBLE(Y(I)) /(X12*X23)
      TERM3 = DBLE(Y(I+1))/(X13*X23)
      A = TERM1+TERM2+TERM3
      B = -(X2+X3)*TERM1 - (X1+X3)*TERM2 - (X1+X2)*TERM3
      C = X2*X3*TERM1 + X1*X3*TERM2 + X1*X2*TERM3
      IF (I-ISTART) 30,30,35
   30 CA = A
      CB = B
      CC = C
      GO TO 40
   35 CA = 0.5*(A+CA)
      CB = 0.5*(B+CB)
      CC = 0.5*(C+CC)
   40 SYU = X2
      SYU2= SYU*SYU
      SYU3= SYU2*SYU
      SUM = SUM + CA*(SYU3-SYL3)/R3  + CB*RP5*(SYU2-SYL2) + CC*(SYU-SYL)
      CA  = A
      CB  = B
      CC  = C
      SYL = SYU
      SYL2= SYU2
      SYL3= SYU3
   50 CONTINUE
      SYU = XUP
      ANS = SUM + CA*(SYU**3-SYL3)/R3 + CB*RP5*(SYU**2-SYL2)
     1  + CC*(SYU-SYL)
  100 RETURN
  200 IERR=2
      CALL XERMSG ('SLATEC', 'AVINT',
     +   'THE UPPER LIMIT OF INTEGRATION WAS NOT GREATER THAN THE ' //
     +   'LOWER LIMIT.', 4, 1)
      RETURN
  205 IERR=3
      CALL XERMSG ('SLATEC', 'AVINT',
     +   'THERE WERE LESS THAN THREE FUNCTION VALUES BETWEEN THE ' //
     +   'LIMITS OF INTEGRATION.', 4, 1)
      RETURN
  210 IERR=4
      CALL XERMSG ('SLATEC', 'AVINT',
     +   'THE ABSCISSAS WERE NOT STRICTLY INCREASING.  MUST HAVE ' //
     +   'X(I-1) .LT. X(I) FOR ALL I.', 4, 1)
      RETURN
  215 IERR=5
      CALL XERMSG ('SLATEC', 'AVINT',
     +   'LESS THAN TWO FUNCTION VALUES WERE SUPPLIED.', 4, 1)
      RETURN
      END
