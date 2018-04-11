      SUBROUTINE SDAWTS (NEQ, IWT, RTOL, ATOL, Y, WT, RPAR, IPAR)
C
      INTEGER  NEQ, IWT, IPAR(*)
      REAL  RTOL(*), ATOL(*), Y(*), WT(*), RPAR(*)
C
      INTEGER  I
      REAL  ATOLI, RTOLI
C
C***FIRST EXECUTABLE STATEMENT  SDAWTS
      RTOLI=RTOL(1)
      ATOLI=ATOL(1)
      DO 20 I=1,NEQ
         IF (IWT .EQ.0) GO TO 10
           RTOLI=RTOL(I)
           ATOLI=ATOL(I)
10         WT(I)=RTOLI*ABS(Y(I))+ATOLI
20         CONTINUE
      RETURN
C-----------END OF SUBROUTINE SDAWTS------------------------------------
      END
