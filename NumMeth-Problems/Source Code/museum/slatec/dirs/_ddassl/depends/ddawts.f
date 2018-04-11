      SUBROUTINE DDAWTS (NEQ, IWT, RTOL, ATOL, Y, WT, RPAR, IPAR)
C
      INTEGER  NEQ, IWT, IPAR(*)
      DOUBLE PRECISION  RTOL(*), ATOL(*), Y(*), WT(*), RPAR(*)
C
      INTEGER  I
      DOUBLE PRECISION  ATOLI, RTOLI
C
C***FIRST EXECUTABLE STATEMENT  DDAWTS
      RTOLI=RTOL(1)
      ATOLI=ATOL(1)
      DO 20 I=1,NEQ
         IF (IWT .EQ.0) GO TO 10
           RTOLI=RTOL(I)
           ATOLI=ATOL(I)
10         WT(I)=RTOLI*ABS(Y(I))+ATOLI
20         CONTINUE
      RETURN
C-----------END OF SUBROUTINE DDAWTS------------------------------------
      END
