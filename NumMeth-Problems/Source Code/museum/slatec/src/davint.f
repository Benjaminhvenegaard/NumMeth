      SUBROUTINE DAVINT (X, Y, N, XLO, XUP, ANS, IERR)
C
      INTEGER I, IERR, INLFT, INRT, ISTART, ISTOP, N
      DOUBLE PRECISION A, ANS, B, C, CA, CB, CC, FL, FR, R3, RP5,
     1     SLOPE, SUM, SYL, SYL2, SYL3, SYU, SYU2, SYU3, TERM1, TERM2,
     2     TERM3, X, X1, X12, X13, X2, X23, X3, XLO, XUP, Y
      DIMENSION X(*),Y(*)
C     BEGIN BLOCK PERMITTING ...EXITS TO 190
C        BEGIN BLOCK PERMITTING ...EXITS TO 180
C***FIRST EXECUTABLE STATEMENT  DAVINT
            IERR = 1
            ANS = 0.0D0
            IF (XLO .GT. XUP) GO TO 160
               IF (XLO .EQ. XUP) GO TO 150
                  IF (N .GE. 2) GO TO 10
                     IERR = 5
                     CALL XERMSG ('SLATEC', 'DAVINT',
     +                  'LESS THAN TWO FUNCTION VALUES WERE SUPPLIED.',
     +                  4, 1)
C     ...............EXIT
                     GO TO 190
   10             CONTINUE
                  DO 20 I = 2, N
C        ............EXIT
                     IF (X(I) .LE. X(I-1)) GO TO 180
C                 ...EXIT
                     IF (X(I) .GT. XUP) GO TO 30
   20             CONTINUE
   30             CONTINUE
                  IF (N .GE. 3) GO TO 40
C
C                    SPECIAL N=2 CASE
                     SLOPE = (Y(2) - Y(1))/(X(2) - X(1))
                     FL = Y(1) + SLOPE*(XLO - X(1))
                     FR = Y(2) + SLOPE*(XUP - X(2))
                     ANS = 0.5D0*(FL + FR)*(XUP - XLO)
C     ...............EXIT
                     GO TO 190
   40             CONTINUE
                  IF (X(N-2) .GE. XLO) GO TO 50
                     IERR = 3
                     CALL XERMSG ('SLATEC', 'DAVINT',
     +                  'THERE WERE LESS THAN THREE FUNCTION VALUES ' //
     +                  'BETWEEN THE LIMITS OF INTEGRATION.', 4, 1)
C     ...............EXIT
                     GO TO 190
   50             CONTINUE
                  IF (X(3) .LE. XUP) GO TO 60
                     IERR = 3
                     CALL XERMSG ('SLATEC', 'DAVINT',
     +                  'THERE WERE LESS THAN THREE FUNCTION VALUES ' //
     +                  'BETWEEN THE LIMITS OF INTEGRATION.', 4, 1)
C     ...............EXIT
                     GO TO 190
   60             CONTINUE
                  I = 1
   70             IF (X(I) .GE. XLO) GO TO 80
                     I = I + 1
                  GO TO 70
   80             CONTINUE
                  INLFT = I
                  I = N
   90             IF (X(I) .LE. XUP) GO TO 100
                     I = I - 1
                  GO TO 90
  100             CONTINUE
                  INRT = I
                  IF ((INRT - INLFT) .GE. 2) GO TO 110
                     IERR = 3
                     CALL XERMSG ('SLATEC', 'DAVINT',
     +                  'THERE WERE LESS THAN THREE FUNCTION VALUES ' //
     +                  'BETWEEN THE LIMITS OF INTEGRATION.', 4, 1)
C     ...............EXIT
                     GO TO 190
  110             CONTINUE
                  ISTART = INLFT
                  IF (INLFT .EQ. 1) ISTART = 2
                  ISTOP = INRT
                  IF (INRT .EQ. N) ISTOP = N - 1
C
                  R3 = 3.0D0
                  RP5 = 0.5D0
                  SUM = 0.0D0
                  SYL = XLO
                  SYL2 = SYL*SYL
                  SYL3 = SYL2*SYL
C
                  DO 140 I = ISTART, ISTOP
                     X1 = X(I-1)
                     X2 = X(I)
                     X3 = X(I+1)
                     X12 = X1 - X2
                     X13 = X1 - X3
                     X23 = X2 - X3
                     TERM1 = Y(I-1)/(X12*X13)
                     TERM2 = -Y(I)/(X12*X23)
                     TERM3 = Y(I+1)/(X13*X23)
                     A = TERM1 + TERM2 + TERM3
                     B = -(X2 + X3)*TERM1 - (X1 + X3)*TERM2
     1                   - (X1 + X2)*TERM3
                     C = X2*X3*TERM1 + X1*X3*TERM2 + X1*X2*TERM3
                     IF (I .GT. ISTART) GO TO 120
                        CA = A
                        CB = B
                        CC = C
                     GO TO 130
  120                CONTINUE
                        CA = 0.5D0*(A + CA)
                        CB = 0.5D0*(B + CB)
                        CC = 0.5D0*(C + CC)
  130                CONTINUE
                     SYU = X2
                     SYU2 = SYU*SYU
                     SYU3 = SYU2*SYU
                     SUM = SUM + CA*(SYU3 - SYL3)/R3
     1                     + CB*RP5*(SYU2 - SYL2) + CC*(SYU - SYL)
                     CA = A
                     CB = B
                     CC = C
                     SYL = SYU
                     SYL2 = SYU2
                     SYL3 = SYU3
  140             CONTINUE
                  SYU = XUP
                  ANS = SUM + CA*(SYU**3 - SYL3)/R3
     1                  + CB*RP5*(SYU**2 - SYL2) + CC*(SYU - SYL)
  150          CONTINUE
            GO TO 170
  160       CONTINUE
               IERR = 2
               CALL XERMSG ('SLATEC', 'DAVINT',
     +            'THE UPPER LIMIT OF INTEGRATION WAS NOT GREATER ' //
     +            'THAN THE LOWER LIMIT.', 4, 1)
  170       CONTINUE
C     ......EXIT
            GO TO 190
  180    CONTINUE
         IERR = 4
         CALL XERMSG ('SLATEC', 'DAVINT',
     +      'THE ABSCISSAS WERE NOT STRICTLY INCREASING.  MUST HAVE ' //
     +      'X(I-1) .LT. X(I) FOR ALL I.', 4, 1)
  190 CONTINUE
      RETURN
      END
