      SUBROUTINE DGEFS (A, LDA, N, V, ITASK, IND, WORK, IWORK)
C
      INTEGER LDA,N,ITASK,IND,IWORK(*)
      DOUBLE PRECISION A(LDA,*),V(*),WORK(*),D1MACH
      DOUBLE PRECISION RCOND
      CHARACTER*8 XERN1, XERN2
C***FIRST EXECUTABLE STATEMENT  DGEFS
      IF (LDA.LT.N) THEN
         IND = -1
         WRITE (XERN1, '(I8)') LDA
         WRITE (XERN2, '(I8)') N
         CALL XERMSG ('SLATEC', 'DGEFS', 'LDA = ' // XERN1 //
     *      ' IS LESS THAN N = ' // XERN2, -1, 1)
         RETURN
      ENDIF
C
      IF (N.LE.0) THEN
         IND = -2
         WRITE (XERN1, '(I8)') N
         CALL XERMSG ('SLATEC', 'DGEFS', 'N = ' // XERN1 //
     *      ' IS LESS THAN 1', -2, 1)
         RETURN
      ENDIF
C
      IF (ITASK.LT.1) THEN
         IND = -3
         WRITE (XERN1, '(I8)') ITASK
         CALL XERMSG ('SLATEC', 'DGEFS', 'ITASK = ' // XERN1 //
     *      ' IS LESS THAN 1', -3, 1)
         RETURN
      ENDIF
C
      IF (ITASK.EQ.1) THEN
C
C        FACTOR MATRIX A INTO LU
C
         CALL DGECO(A,LDA,N,IWORK,RCOND,WORK)
C
C        CHECK FOR COMPUTATIONALLY SINGULAR MATRIX
C
         IF (RCOND.EQ.0.0D0) THEN
            IND = -4
            CALL XERMSG ('SLATEC', 'DGEFS',
     *         'SINGULAR MATRIX A - NO SOLUTION', -4, 1)
            RETURN
         ENDIF
C
C        COMPUTE IND (ESTIMATE OF NO. OF SIGNIFICANT DIGITS)
C        AND CHECK FOR IND GREATER THAN ZERO
C
         IND = -LOG10(D1MACH(4)/RCOND)
         IF (IND.LE.0) THEN
            IND=-10
            CALL XERMSG ('SLATEC', 'DGEFS',
     *         'SOLUTION MAY HAVE NO SIGNIFICANCE', -10, 0)
         ENDIF
      ENDIF
C
C     SOLVE AFTER FACTORING
C
      CALL DGESL(A,LDA,N,IWORK,V,0)
      RETURN
      END
