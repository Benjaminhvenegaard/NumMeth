      SUBROUTINE ORTHOL (A, M, N, NRDA, IFLAG, IRANK, ISCALE, DIAG,
     +   KPIVOT, SCALES, COLS, CS)
      DIMENSION A(NRDA,*),DIAG(*),KPIVOT(*),COLS(*),CS(*),SCALES(*)
C
C **********************************************************************
C
C     MACHINE PRECISION (COMPUTER UNIT ROUNDOFF VALUE) IS DEFINED
C     BY THE FUNCTION R1MACH.
C
C***FIRST EXECUTABLE STATEMENT  ORTHOL
      URO = R1MACH(3)
C
C **********************************************************************
C
      IF (M .GE. N  .AND.  N .GE. 1  .AND.  NRDA .GE. M) GO TO 1
      IFLAG=2
      CALL XERMSG ('SLATEC', 'ORTHOL', 'INVALID INPUT PARAMETERS.', 2,
     +   1)
      RETURN
C
    1 ACC=10.*URO
      IF (IFLAG .LT. 0) ACC=MAX(ACC,10.**IFLAG)
      SRURO=SQRT(URO)
      IFLAG=1
      IRANK=N
C
C     COMPUTE NORM**2 OF JTH COLUMN AND A MATRIX NORM
C
      ANORM=0.
      DO 2 J=1,N
         KPIVOT(J)=J
         COLS(J)=SDOT(M,A(1,J),1,A(1,J),1)
         CS(J)=COLS(J)
         ANORM=ANORM+COLS(J)
    2 CONTINUE
C
C     PERFORM COLUMN SCALING ON A WHEN SPECIFIED
C
      CALL CSCALE(A,NRDA,M,N,COLS,CS,DUM,DUM,ANORM,SCALES,ISCALE,0)
C
      ANORM=SQRT(ANORM)
C
C
C     CONSTRUCTION OF UPPER TRIANGULAR MATRIX AND RECORDING OF
C     ORTHOGONAL TRANSFORMATIONS
C
C
      DO 50 K=1,N
         MK=M-K+1
         IF (K .EQ. N) GO TO 25
         KP=K+1
C
C        SEARCHING FOR PIVOTAL COLUMN
C
         DO 10 J=K,N
            IF (COLS(J) .GE. SRURO*CS(J)) GO TO 5
            COLS(J)=SDOT(MK,A(K,J),1,A(K,J),1)
            CS(J)=COLS(J)
    5       IF (J .EQ. K) GO TO 7
            IF (SIGMA .GE. 0.99*COLS(J)) GO TO 10
    7       SIGMA=COLS(J)
            JCOL=J
   10    CONTINUE
         IF (JCOL .EQ. K) GO TO 25
C
C        PERFORM COLUMN INTERCHANGE
C
         L=KPIVOT(K)
         KPIVOT(K)=KPIVOT(JCOL)
         KPIVOT(JCOL)=L
         COLS(JCOL)=COLS(K)
         COLS(K)=SIGMA
         CSS=CS(K)
         CS(K)=CS(JCOL)
         CS(JCOL)=CSS
         SC=SCALES(K)
         SCALES(K)=SCALES(JCOL)
         SCALES(JCOL)=SC
         DO 20 L=1,M
            ASAVE=A(L,K)
            A(L,K)=A(L,JCOL)
   20       A(L,JCOL)=ASAVE
C
C        CHECK RANK OF THE MATRIX
C
   25    SIG=SDOT(MK,A(K,K),1,A(K,K),1)
         DIAGK=SQRT(SIG)
         IF (DIAGK .GT. ACC*ANORM) GO TO 30
C
C        RANK DEFICIENT PROBLEM
         IFLAG=3
         IRANK=K-1
         CALL XERMSG ('SLATEC', 'ORTHOL',
     +      'RANK OF MATRIX IS LESS THAN THE NUMBER OF COLUMNS.', 1, 1)
         RETURN
C
C        CONSTRUCT AND APPLY TRANSFORMATION TO MATRIX A
C
   30    AKK=A(K,K)
         IF (AKK .GT. 0.) DIAGK=-DIAGK
         DIAG(K)=DIAGK
         A(K,K)=AKK-DIAGK
         IF (K .EQ. N) GO TO 50
         SAD=DIAGK*AKK-SIG
         DO 40 J=KP,N
            AS=SDOT(MK,A(K,K),1,A(K,J),1)/SAD
            DO 35 L=K,M
   35          A(L,J)=A(L,J)+AS*A(L,K)
   40       COLS(J)=COLS(J)-A(K,J)**2
   50 CONTINUE
C
C
      RETURN
      END
