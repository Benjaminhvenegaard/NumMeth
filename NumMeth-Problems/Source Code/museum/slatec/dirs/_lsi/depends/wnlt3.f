      SUBROUTINE WNLT3 (I, IMAX, M, MDW, IPIVOT, H, W)
      INTEGER I, IMAX, IPIVOT(*), M, MDW
      REAL             H(*), W(MDW,*)
C
      EXTERNAL SSWAP
C
      REAL             T
      INTEGER ITEMP
C
C***FIRST EXECUTABLE STATEMENT  WNLT3
      IF (IMAX.NE.I) THEN
         ITEMP        = IPIVOT(I)
         IPIVOT(I)    = IPIVOT(IMAX)
         IPIVOT(IMAX) = ITEMP
C
         CALL SSWAP(M, W(1,IMAX), 1, W(1,I), 1)
C
         T       = H(IMAX)
         H(IMAX) = H(I)
         H(I)    = T
      ENDIF
      RETURN
      END
