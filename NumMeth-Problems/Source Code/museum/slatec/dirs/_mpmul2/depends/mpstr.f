      SUBROUTINE MPSTR (X, Y)
      COMMON /MPCOM/ B, T, M, LUN, MXR, R(30)
      INTEGER B, T, R, X(*), Y(*)
C***FIRST EXECUTABLE STATEMENT  MPSTR
      DO 10 I = 1, T+2
         Y(I) = X(I)
   10 CONTINUE
      RETURN
      END
