      SUBROUTINE CROTG (CA, CB, C, S)
      COMPLEX CA, CB, S
      REAL C
      REAL NORM, SCALE
      COMPLEX ALPHA
C***FIRST EXECUTABLE STATEMENT  CROTG
      IF (ABS(CA) .EQ. 0.0) THEN
        C = 0.0
        S = (1.0,0.0)
        CA = CB
      ELSE
        SCALE = ABS(CA) + ABS(CB)
        NORM = SCALE * SQRT((ABS(CA/SCALE))**2 + (ABS(CB/SCALE))**2)
        ALPHA = CA /ABS(CA)
        C = ABS(CA) / NORM
        S = ALPHA * CONJG(CB) / NORM
        CA = ALPHA * NORM
      ENDIF
      RETURN
      END
