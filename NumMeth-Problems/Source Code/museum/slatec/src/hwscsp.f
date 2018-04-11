      SUBROUTINE HWSCSP (INTL, TS, TF, M, MBDCND, BDTS, BDTF, RS, RF, N,
     +   NBDCND, BDRS, BDRF, ELMBDA, F, IDIMF, PERTRB, IERROR, W)
C
      DIMENSION       F(IDIMF,*) ,BDTS(*)    ,BDTF(*)    ,BDRS(*)    ,
     1                BDRF(*)    ,W(*)
C***FIRST EXECUTABLE STATEMENT  HWSCSP
      PI = PIMACH(DUM)
      IERROR = 0
      IF (TS.LT.0. .OR. TF.GT.PI) IERROR = 1
      IF (TS .GE. TF) IERROR = 2
      IF (M .LT. 5) IERROR = 3
      IF (MBDCND.LT.1 .OR. MBDCND.GT.9) IERROR = 4
      IF (RS .LT. 0.) IERROR = 5
      IF (RS .GE. RF) IERROR = 6
      IF (N .LT. 5) IERROR = 7
      IF (NBDCND.LT.1 .OR. NBDCND.GT.6) IERROR = 8
      IF (ELMBDA .GT. 0.) IERROR = 9
      IF (IDIMF .LT. M+1) IERROR = 10
      IF (ELMBDA.NE.0. .AND. MBDCND.GE.5) IERROR = 11
      IF (ELMBDA.NE.0. .AND. (NBDCND.EQ.5 .OR. NBDCND.EQ.6)) IERROR = 12
      IF ((MBDCND.EQ.5 .OR. MBDCND.EQ.6 .OR. MBDCND.EQ.9) .AND.
     1    TS.NE.0.) IERROR = 13
      IF (MBDCND.GE.7 .AND. TF.NE.PI) IERROR = 14
      IF (TS.EQ.0. .AND.
     1    (MBDCND.EQ.4 .OR. MBDCND.EQ.8 .OR. MBDCND.EQ.3)) IERROR = 15
      IF (TF.EQ.PI .AND.
     1    (MBDCND.EQ.2 .OR. MBDCND.EQ.3 .OR. MBDCND.EQ.6)) IERROR = 16
      IF (NBDCND.GE.5 .AND. RS.NE.0.) IERROR = 17
      IF (NBDCND.GE.5 .AND. (MBDCND.EQ.1 .OR. MBDCND.EQ.2 .OR.
     1                                    MBDCND.EQ.5 .OR. MBDCND.EQ.7))
     2    IERROR = 18
      IF (IERROR.NE.0 .AND. IERROR.NE.9) RETURN
      NCK = N
      GO TO (101,103,102,103,101,103),NBDCND
  101 NCK = NCK-1
      GO TO 103
  102 NCK = NCK+1
  103 L = 2
      K = 1
  104 L = L+L
      K = K+1
      IF (NCK-L) 105,105,104
  105 L = L+L
      NP1 = N+1
      MP1 = M+1
      I1 = (K-2)*L+K+MAX(2*N,6*M)+13
      I2 = I1+NP1
      I3 = I2+NP1
      I4 = I3+NP1
      I5 = I4+NP1
      I6 = I5+NP1
      I7 = I6+MP1
      I8 = I7+MP1
      I9 = I8+MP1
      I10 = I9+MP1
      W(1) = I10+M
      CALL HWSCS1 (INTL,TS,TF,M,MBDCND,BDTS,BDTF,RS,RF,N,NBDCND,BDRS,
     1             BDRF,ELMBDA,F,IDIMF,PERTRB,W(2),W(I1),W(I2),W(I3),
     2             W(I4),W(I5),W(I6),W(I7),W(I8),W(I9),W(I10))
      RETURN
      END
