      SUBROUTINE CNBIR (ABE, LDA, N, ML, MU, V, ITASK, IND, WORK, IWORK)
C
      INTEGER LDA,N,ITASK,IND,IWORK(*),INFO,J,K,KK,L,M,ML,MU,NC
      COMPLEX ABE(LDA,*),V(*),WORK(N,*),CDCDOT
      REAL XNORM,DNORM,SCASUM,R1MACH
      CHARACTER*8 XERN1, XERN2
C***FIRST EXECUTABLE STATEMENT  CNBIR
      IF (LDA.LT.N) THEN
         IND = -1
         WRITE (XERN1, '(I8)') LDA
         WRITE (XERN2, '(I8)') N
         CALL XERMSG ('SLATEC', 'CNBIR', 'LDA = ' // XERN1 //
     *      ' IS LESS THAN N = ' // XERN2, -1, 1)
         RETURN
      ENDIF
C
      IF (N.LE.0) THEN
         IND = -2
         WRITE (XERN1, '(I8)') N
         CALL XERMSG ('SLATEC', 'CNBIR', 'N = ' // XERN1 //
     *      ' IS LESS THAN 1', -2, 1)
         RETURN
      ENDIF
C
      IF (ITASK.LT.1) THEN
         IND = -3
         WRITE (XERN1, '(I8)') ITASK
         CALL XERMSG ('SLATEC', 'CNBIR', 'ITASK = ' // XERN1 //
     *      ' IS LESS THAN 1', -3, 1)
         RETURN
      ENDIF
C
      IF (ML.LT.0 .OR. ML.GE.N) THEN
         IND = -5
         WRITE (XERN1, '(I8)') ML
         CALL XERMSG ('SLATEC', 'CNBIR',
     *      'ML = ' // XERN1 // ' IS OUT OF RANGE', -5, 1)
         RETURN
      ENDIF
C
      IF (MU.LT.0 .OR. MU.GE.N) THEN
         IND = -6
         WRITE (XERN1, '(I8)') MU
         CALL XERMSG ('SLATEC', 'CNBIR',
     *      'MU = ' // XERN1 // ' IS OUT OF RANGE', -6, 1)
         RETURN
      ENDIF
C
      NC = 2*ML+MU+1
      IF (ITASK.EQ.1) THEN
C
C        MOVE MATRIX ABE TO WORK
C
         M=ML+MU+1
         DO 10 J=1,M
            CALL CCOPY(N,ABE(1,J),1,WORK(1,J),1)
   10    CONTINUE
C
C        FACTOR MATRIX A INTO LU
         CALL CNBFA(WORK,N,N,ML,MU,IWORK,INFO)
C
C        CHECK FOR COMPUTATIONALLY SINGULAR MATRIX
         IF (INFO.NE.0) THEN
            IND=-4
            CALL XERMSG ('SLATEC', 'CNBIR',
     *         'SINGULAR MATRIX A - NO SOLUTION', -4, 1)
            RETURN
         ENDIF
      ENDIF
C
C     SOLVE WHEN FACTORING COMPLETE
C     MOVE VECTOR B TO WORK
C
      CALL CCOPY(N,V(1),1,WORK(1,NC+1),1)
      CALL CNBSL(WORK,N,N,ML,MU,IWORK,V,0)
C
C     FORM NORM OF X0
C
      XNORM = SCASUM(N,V(1),1)
      IF (XNORM.EQ.0.0) THEN
         IND = 75
         RETURN
      ENDIF
C
C     COMPUTE  RESIDUAL
C
      DO 40 J=1,N
         K  = MAX(1,ML+2-J)
         KK = MAX(1,J-ML)
         L  = MIN(J-1,ML)+MIN(N-J,MU)+1
         WORK(J,NC+1) = CDCDOT(L,-WORK(J,NC+1),ABE(J,K),LDA,V(KK),1)
   40 CONTINUE
C
C     SOLVE A*DELTA=R
C
      CALL CNBSL(WORK,N,N,ML,MU,IWORK,WORK(1,NC+1),0)
C
C     FORM NORM OF DELTA
C
      DNORM = SCASUM(N,WORK(1,NC+1),1)
C
C     COMPUTE IND (ESTIMATE OF NO. OF SIGNIFICANT DIGITS)
C     AND CHECK FOR IND GREATER THAN ZERO
C
      IND = -LOG10(MAX(R1MACH(4),DNORM/XNORM))
      IF (IND.LE.0) THEN
         IND = -10
         CALL XERMSG ('SLATEC', 'CNBIR',
     *      'SOLUTION MAY HAVE NO SIGNIFICANCE', -10, 0)
      ENDIF
      RETURN
      END
