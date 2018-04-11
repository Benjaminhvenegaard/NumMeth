      SUBROUTINE WNLT1 (I, LEND, MEND, IR, MDW, RECALC, IMAX, HBAR, H,
     +   SCALE, W)
      INTEGER I, IMAX, IR, LEND, MDW, MEND
      REAL             H(*), HBAR, SCALE(*), W(MDW,*)
      LOGICAL RECALC
C
      EXTERNAL ISAMAX
      INTEGER ISAMAX
C
      INTEGER J, K
C
C***FIRST EXECUTABLE STATEMENT  WNLT1
      IF (IR.NE.1 .AND. (.NOT.RECALC)) THEN
C
C        Update column SS=sum of squares.
C
         DO 10 J=I,LEND
            H(J) = H(J) - SCALE(IR-1)*W(IR-1,J)**2
   10    CONTINUE
C
C        Test for numerical accuracy.
C
         IMAX = ISAMAX(LEND-I+1, H(I), 1) + I - 1
         RECALC = (HBAR+1.E-3*H(IMAX)) .EQ. HBAR
      ENDIF
C
C     If required, recalculate column SS, using rows IR through MEND.
C
      IF (RECALC) THEN
         DO 30 J=I,LEND
            H(J) = 0.E0
            DO 20 K=IR,MEND
               H(J) = H(J) + SCALE(K)*W(K,J)**2
   20       CONTINUE
   30    CONTINUE
C
C        Find column with largest SS.
C
         IMAX = ISAMAX(LEND-I+1, H(I), 1) + I - 1
         HBAR = H(IMAX)
      ENDIF
      RETURN
      END
