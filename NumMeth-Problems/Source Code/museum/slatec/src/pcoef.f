      SUBROUTINE PCOEF (L, C, TC, A)
C
      DIMENSION A(*), TC(*)
C***FIRST EXECUTABLE STATEMENT  PCOEF
      LL = ABS(L)
      LLP1 = LL + 1
      CALL PVALUE (LL,LL,C,TC(1),TC(2),A)
      IF (LL .LT. 2) GO TO 2
      FAC = 1.0
      DO 1 I = 3,LLP1
        FAC = FAC*(I-1)
 1      TC(I) = TC(I)/FAC
 2    IF (L .GE. 0) GO TO 4
      NR = LLP1/2
      LLP2 = LL + 2
      DO 3 I = 1,NR
        SAVE = TC(I)
        NEW = LLP2 - I
        TC(I) = TC(NEW)
 3      TC(NEW) = SAVE
 4    RETURN
      END
