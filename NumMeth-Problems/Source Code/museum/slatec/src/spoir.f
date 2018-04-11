      SUBROUTINE SPOIR (A, LDA, N, V, ITASK, IND, WORK)
C
      INTEGER LDA,N,ITASK,IND,INFO,J
      REAL A(LDA,*),V(*),WORK(N,*),SASUM,XNORM,DNORM,R1MACH
      DOUBLE PRECISION DSDOT
      CHARACTER*8 XERN1, XERN2
C***FIRST EXECUTABLE STATEMENT  SPOIR
      IF (LDA.LT.N) THEN
         IND = -1
         WRITE (XERN1, '(I8)') LDA
         WRITE (XERN2, '(I8)') N
         CALL XERMSG ('SLATEC', 'SPOIR', 'LDA = ' // XERN1 //
     *      ' IS LESS THAN N = ' // XERN2, -1, 1)
         RETURN
      ENDIF
C
      IF (N.LE.0) THEN
         IND = -2
         WRITE (XERN1, '(I8)') N
         CALL XERMSG ('SLATEC', 'SPOIR', 'N = ' // XERN1 //
     *      ' IS LESS THAN 1', -2, 1)
         RETURN
      ENDIF
C
      IF (ITASK.LT.1) THEN
         IND = -3
         WRITE (XERN1, '(I8)') ITASK
         CALL XERMSG ('SLATEC', 'SPOIR', 'ITASK = ' // XERN1 //
     *      ' IS LESS THAN 1', -3, 1)
         RETURN
      ENDIF
C
      IF (ITASK.EQ.1) THEN
C
C        MOVE MATRIX A TO WORK
C
         DO 10 J=1,N
            CALL SCOPY(N,A(1,J),1,WORK(1,J),1)
   10    CONTINUE
C
C        FACTOR MATRIX A INTO R
         CALL SPOFA(WORK,N,N,INFO)
C
C        CHECK FOR  SINGULAR OR NOT POS.DEF. MATRIX
         IF (INFO.NE.0) THEN
            IND = -4
            CALL XERMSG ('SLATEC', 'SPOIR',
     *         'SINGULAR OR NOT POSITIVE DEFINITE - NO SOLUTION', -4, 1)
            RETURN
         ENDIF
      ENDIF
C
C     SOLVE AFTER FACTORING
C     MOVE VECTOR B TO WORK
C
      CALL SCOPY(N,V(1),1,WORK(1,N+1),1)
      CALL SPOSL(WORK,N,N,V)
C
C     FORM NORM OF X0
C
      XNORM = SASUM(N,V(1),1)
      IF (XNORM.EQ.0.0) THEN
         IND = 75
         RETURN
      ENDIF
C
C     COMPUTE  RESIDUAL
C
      DO 40 J=1,N
         WORK(J,N+1) = -WORK(J,N+1)
     1                 +DSDOT(J-1,A(1,J),1,V(1),1)
     2                 +DSDOT(N-J+1,A(J,J),LDA,V(J),1)
   40 CONTINUE
C
C     SOLVE A*DELTA=R
C
      CALL SPOSL(WORK,N,N,WORK(1,N+1))
C
C     FORM NORM OF DELTA
C
      DNORM = SASUM(N,WORK(1,N+1),1)
C
C     COMPUTE IND (ESTIMATE OF NO. OF SIGNIFICANT DIGITS)
C     AND CHECK FOR IND GREATER THAN ZERO
C
      IND = -LOG10(MAX(R1MACH(4),DNORM/XNORM))
      IF (IND.LE.0) THEN
         IND = -10
         CALL XERMSG ('SLATEC', 'SPOIR',
     *      'SOLUTION MAY HAVE NO SIGNIFICANCE', -10, 0)
      ENDIF
      RETURN
      END
