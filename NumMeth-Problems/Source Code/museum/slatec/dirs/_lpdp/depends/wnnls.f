      SUBROUTINE WNNLS (W, MDW, ME, MA, N, L, PRGOPT, X, RNORM, MODE,
     +   IWORK, WORK)
      REAL              PRGOPT(*), RNORM, W(MDW,*), WORK(*), X(*)
      INTEGER IWORK(*)
      CHARACTER*8 XERN1
C
C
C***FIRST EXECUTABLE STATEMENT  WNNLS
      MODE = 0
      IF (MA+ME.LE.0 .OR. N.LE.0) RETURN
      IF (IWORK(1).GT.0) THEN
         LW = ME + MA + 5*N
         IF (IWORK(1).LT.LW) THEN
            WRITE (XERN1, '(I8)') LW
            CALL XERMSG ('SLATEC', 'WNNLS', 'INSUFFICIENT STORAGE ' //
     *         'ALLOCATED FOR WORK(*), NEED LW = ' // XERN1, 2, 1)
            MODE = 2
            RETURN
         ENDIF
      ENDIF
C
      IF (IWORK(2).GT.0) THEN
         LIW = ME + MA + N
         IF (IWORK(2).LT.LIW) THEN
            WRITE (XERN1, '(I8)') LIW
            CALL XERMSG ('SLATEC', 'WNNLS', 'INSUFFICIENT STORAGE ' //
     *         'ALLOCATED FOR IWORK(*), NEED LIW = ' // XERN1, 2, 1)
            MODE = 2
            RETURN
         ENDIF
      ENDIF
C
      IF (MDW.LT.ME+MA) THEN
         CALL XERMSG ('SLATEC', 'WNNLS',
     *      'THE VALUE MDW.LT.ME+MA IS AN ERROR', 1, 1)
         MODE = 2
         RETURN
      ENDIF
C
      IF (L.LT.0 .OR. L.GT.N) THEN
         CALL XERMSG ('SLATEC', 'WNNLS',
     *      'L.GE.0 .AND. L.LE.N IS REQUIRED', 2, 1)
         MODE = 2
         RETURN
      ENDIF
C
C     THE PURPOSE OF THIS SUBROUTINE IS TO BREAK UP THE ARRAYS
C     WORK(*) AND IWORK(*) INTO SEPARATE WORK ARRAYS
C     REQUIRED BY THE MAIN SUBROUTINE WNLSM( ).
C
      L1 = N + 1
      L2 = L1 + N
      L3 = L2 + ME + MA
      L4 = L3 + N
      L5 = L4 + N
C
      CALL WNLSM(W, MDW, ME, MA, N, L, PRGOPT, X, RNORM, MODE, IWORK,
     *           IWORK(L1), WORK(1), WORK(L1), WORK(L2), WORK(L3),
     *           WORK(L4), WORK(L5))
      RETURN
      END
