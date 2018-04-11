      SUBROUTINE HSTCSP (INTL, A, B, M, MBDCND, BDA, BDB, C, D, N,
     +   NBDCND, BDC, BDD, ELMBDA, F, IDIMF, PERTRB, IERROR, W)
C
C
      DIMENSION       F(IDIMF,*) ,BDA(*)     ,BDB(*)     ,BDC(*)     ,
     1                BDD(*)     ,W(*)
C***FIRST EXECUTABLE STATEMENT  HSTCSP
      PI = PIMACH(DUM)
C
C     CHECK FOR INVALID INPUT PARAMETERS
C
      IERROR = 0
      IF (A.LT.0. .OR. B.GT.PI) IERROR = 1
      IF (A .GE. B) IERROR = 2
      IF (MBDCND.LT.1 .OR. MBDCND.GT.9) IERROR = 3
      IF (C .LT. 0.) IERROR = 4
      IF (C .GE. D) IERROR = 5
      IF (NBDCND.LT.1 .OR. NBDCND.GT.6) IERROR = 6
      IF (N .LT. 5) IERROR = 7
      IF ((NBDCND.EQ.5 .OR. NBDCND.EQ.6) .AND. (MBDCND.EQ.1 .OR.
     1    MBDCND.EQ.2 .OR. MBDCND.EQ.4 .OR. MBDCND.EQ.5 .OR.
     2                                                     MBDCND.EQ.7))
     3    IERROR = 8
      IF (C.GT.0. .AND. NBDCND.GE.5) IERROR = 9
      IF (IDIMF .LT. M) IERROR = 11
      IF (M .LT. 5) IERROR = 12
      IF (A.EQ.0. .AND. MBDCND.NE.5 .AND. MBDCND.NE.6 .AND. MBDCND.NE.9)
     1    IERROR = 13
      IF (B.EQ.PI .AND. MBDCND.LE.6) IERROR = 14
      IF (A.GT.0. .AND. (MBDCND.EQ.5 .OR. MBDCND.EQ.6 .OR. MBDCND.EQ.9))
     1    IERROR = 15
      IF (B.LT.PI .AND. MBDCND.GE.7) IERROR = 16
      IF (ELMBDA.NE.0. .AND. NBDCND.GE.5) IERROR = 17
      IF (IERROR .NE. 0) GO TO 101
      IWBM = M+1
      IWCM = IWBM+M
      IWAN = IWCM+M
      IWBN = IWAN+N
      IWCN = IWBN+N
      IWSNTH = IWCN+N
      IWRSQ = IWSNTH+M
      IWWRK = IWRSQ+N
      IERR1 = 0
      CALL HSTCS1 (INTL,A,B,M,MBDCND,BDA,BDB,C,D,N,NBDCND,BDC,BDD,
     1             ELMBDA,F,IDIMF,PERTRB,IERR1,W,W(IWBM),W(IWCM),
     2             W(IWAN),W(IWBN),W(IWCN),W(IWSNTH),W(IWRSQ),W(IWWRK))
      W(1) = W(IWWRK)+IWWRK-1
      IERROR = IERR1
  101 CONTINUE
      RETURN
      END
