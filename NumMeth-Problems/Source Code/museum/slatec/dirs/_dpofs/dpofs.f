      SUBROUTINE DPOFS (A, LDA, N, V, ITASK, IND, WORK)
C
      INTEGER LDA,N,ITASK,IND,INFO
      DOUBLE PRECISION A(LDA,*),V(*),WORK(*),D1MACH
      DOUBLE PRECISION RCOND
      CHARACTER*8 XERN1, XERN2
C***FIRST EXECUTABLE STATEMENT  DPOFS
      IF (LDA.LT.N) THEN
         IND = -1
         WRITE (XERN1, '(I8)') LDA
         WRITE (XERN2, '(I8)') N
         CALL XERMSG ('SLATEC', 'DPOFS', 'LDA = ' // XERN1 //
     *      ' IS LESS THAN N = ' // XERN2, -1, 1)
         RETURN
      ENDIF
C
      IF (N.LE.0) THEN
         IND = -2
         WRITE (XERN1, '(I8)') N
         CALL XERMSG ('SLATEC', 'DPOFS', 'N = ' // XERN1 //
     *      ' IS LESS THAN 1', -2, 1)
         RETURN
      ENDIF
C
      IF (ITASK.LT.1) THEN
         IND = -3
         WRITE (XERN1, '(I8)') ITASK
         CALL XERMSG ('SLATEC', 'DPOFS', 'ITASK = ' // XERN1 //
     *      ' IS LESS THAN 1', -3, 1)
         RETURN
      ENDIF
C
      IF (ITASK.EQ.1) THEN
C
C        FACTOR MATRIX A INTO R
C
         CALL DPOCO(A,LDA,N,RCOND,WORK,INFO)
C
C        CHECK FOR POSITIVE DEFINITE MATRIX
C
         IF (INFO.NE.0) THEN
            IND = -4
            CALL XERMSG ('SLATEC', 'DPOFS',
     *         'SINGULAR OR NOT POSITIVE DEFINITE - NO SOLUTION', -4, 1)
            RETURN
         ENDIF
C
C        COMPUTE IND (ESTIMATE OF NO. OF SIGNIFICANT DIGITS)
C        AND CHECK FOR IND GREATER THAN ZERO
C
         IND = -LOG10(D1MACH(4)/RCOND)
         IF (IND.EQ.0) THEN
            IND = -10
            CALL XERMSG ('SLATEC', 'DPOFS',
     *         'SOLUTION MAY HAVE NO SIGNIFICANCE', -10, 0)
         ENDIF
      ENDIF
C
C     SOLVE AFTER FACTORING
C
      CALL DPOSL(A,LDA,N,V)
      RETURN
      END
