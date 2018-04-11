      DOUBLE PRECISION FUNCTION DVNRMS (N, V, W)
      INTEGER I, N
      DOUBLE PRECISION SUM, V, W
      DIMENSION V(*),W(*)
C***FIRST EXECUTABLE STATEMENT  DVNRMS
      SUM = 0.0D0
      DO 10 I = 1, N
         SUM = SUM + (V(I)/W(I))**2
   10 CONTINUE
      DVNRMS = SQRT(SUM/N)
      RETURN
C     ----------------------- END OF FUNCTION DVNRMS
C     ------------------------
      END
