      SUBROUTINE BNSLV (W, NROWW, NROW, NBANDL, NBANDU, B)
C
      INTEGER NBANDL, NBANDU, NROW, NROWW, I, J, JMAX, MIDDLE, NROWM1
      REAL W(NROWW,*), B(*)
C***FIRST EXECUTABLE STATEMENT  BNSLV
      MIDDLE = NBANDU + 1
      IF (NROW.EQ.1) GO TO 80
      NROWM1 = NROW - 1
      IF (NBANDL.EQ.0) GO TO 30
C                                 FORWARD PASS
C            FOR I=1,2,...,NROW-1, SUBTRACT  RIGHT SIDE(I)*(I-TH COLUMN
C            OF  L )  FROM RIGHT SIDE  (BELOW I-TH ROW) .
      DO 20 I=1,NROWM1
        JMAX = MIN(NBANDL,NROW-I)
        DO 10 J=1,JMAX
          B(I+J) = B(I+J) - B(I)*W(MIDDLE+J,I)
   10   CONTINUE
   20 CONTINUE
C                                 BACKWARD PASS
C            FOR I=NROW,NROW-1,...,1, DIVIDE RIGHT SIDE(I) BY I-TH DIAG-
C            ONAL ENTRY OF  U, THEN SUBTRACT  RIGHT SIDE(I)*(I-TH COLUMN
C            OF  U)  FROM RIGHT SIDE  (ABOVE I-TH ROW).
   30 IF (NBANDU.GT.0) GO TO 50
C                                A  IS LOWER TRIANGULAR .
      DO 40 I=1,NROW
        B(I) = B(I)/W(1,I)
   40 CONTINUE
      RETURN
   50 I = NROW
   60 B(I) = B(I)/W(MIDDLE,I)
      JMAX = MIN(NBANDU,I-1)
      DO 70 J=1,JMAX
        B(I-J) = B(I-J) - B(I)*W(MIDDLE-J,I)
   70 CONTINUE
      I = I - 1
      IF (I.GT.1) GO TO 60
   80 B(1) = B(1)/W(MIDDLE,1)
      RETURN
      END
