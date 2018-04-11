      SUBROUTINE HSTCYL (A, B, M, MBDCND, BDA, BDB, C, D, N, NBDCND,
     +   BDC, BDD, ELMBDA, F, IDIMF, PERTRB, IERROR, W)
C
C
      DIMENSION       F(IDIMF,*) ,BDA(*)     ,BDB(*)     ,BDC(*)     ,
     1                BDD(*)     ,W(*)
C***FIRST EXECUTABLE STATEMENT  HSTCYL
      IERROR = 0
      IF (A .LT. 0.) IERROR = 1
      IF (A .GE. B) IERROR = 2
      IF (MBDCND.LE.0 .OR. MBDCND.GE.7) IERROR = 3
      IF (C .GE. D) IERROR = 4
      IF (N .LE. 2) IERROR = 5
      IF (NBDCND.LT.0 .OR. NBDCND.GE.5) IERROR = 6
      IF (A.EQ.0. .AND. MBDCND.NE.5 .AND. MBDCND.NE.6) IERROR = 7
      IF (A.GT.0. .AND. MBDCND.GE.5) IERROR = 8
      IF (IDIMF .LT. M) IERROR = 10
      IF (M .LE. 2) IERROR = 9
      IF (A.EQ.0. .AND. MBDCND.GE.5 .AND. ELMBDA.NE.0.) IERROR = 12
      IF (IERROR .NE. 0) RETURN
      DELTAR = (B-A)/M
      DLRSQ = DELTAR**2
      DELTHT = (D-C)/N
      DLTHSQ = DELTHT**2
      NP = NBDCND+1
C
C     DEFINE A,B,C COEFFICIENTS IN W-ARRAY.
C
      IWB = M
      IWC = IWB+M
      IWR = IWC+M
      DO 101 I=1,M
         J = IWR+I
         W(J) = A+(I-0.5)*DELTAR
         W(I) = (A+(I-1)*DELTAR)/(DLRSQ*W(J))
         K = IWC+I
         W(K) = (A+I*DELTAR)/(DLRSQ*W(J))
         K = IWB+I
         W(K) = ELMBDA/W(J)**2-2./DLRSQ
  101 CONTINUE
C
C     ENTER BOUNDARY DATA FOR R-BOUNDARIES.
C
      GO TO (102,102,104,104,106,106),MBDCND
  102 A1 = 2.*W(1)
      W(IWB+1) = W(IWB+1)-W(1)
      DO 103 J=1,N
         F(1,J) = F(1,J)-A1*BDA(J)
  103 CONTINUE
      GO TO 106
  104 A1 = DELTAR*W(1)
      W(IWB+1) = W(IWB+1)+W(1)
      DO 105 J=1,N
         F(1,J) = F(1,J)+A1*BDA(J)
  105 CONTINUE
  106 CONTINUE
      GO TO (107,109,109,107,107,109),MBDCND
  107 W(IWC) = W(IWC)-W(IWR)
      A1 = 2.*W(IWR)
      DO 108 J=1,N
         F(M,J) = F(M,J)-A1*BDB(J)
  108 CONTINUE
      GO TO 111
  109 W(IWC) = W(IWC)+W(IWR)
      A1 = DELTAR*W(IWR)
      DO 110 J=1,N
         F(M,J) = F(M,J)-A1*BDB(J)
  110 CONTINUE
C
C     ENTER BOUNDARY DATA FOR THETA-BOUNDARIES.
C
  111 A1 = 2./DLTHSQ
      GO TO (121,112,112,114,114),NP
  112 DO 113 I=1,M
         F(I,1) = F(I,1)-A1*BDC(I)
  113 CONTINUE
      GO TO 116
  114 A1 = 1./DELTHT
      DO 115 I=1,M
         F(I,1) = F(I,1)+A1*BDC(I)
  115 CONTINUE
  116 A1 = 2./DLTHSQ
      GO TO (121,117,119,119,117),NP
  117 DO 118 I=1,M
         F(I,N) = F(I,N)-A1*BDD(I)
  118 CONTINUE
      GO TO 121
  119 A1 = 1./DELTHT
      DO 120 I=1,M
         F(I,N) = F(I,N)-A1*BDD(I)
  120 CONTINUE
  121 CONTINUE
C
C     ADJUST RIGHT SIDE OF SINGULAR PROBLEMS TO INSURE EXISTENCE OF A
C     SOLUTION.
C
      PERTRB = 0.
      IF (ELMBDA) 130,123,122
  122 IERROR = 11
      GO TO 130
  123 GO TO (130,130,124,130,130,124),MBDCND
  124 GO TO (125,130,130,125,130),NP
  125 CONTINUE
      DO 127 I=1,M
         A1 = 0.
         DO 126 J=1,N
            A1 = A1+F(I,J)
  126    CONTINUE
         J = IWR+I
         PERTRB = PERTRB+A1*W(J)
  127 CONTINUE
      PERTRB = PERTRB/(M*N*0.5*(A+B))
      DO 129 I=1,M
         DO 128 J=1,N
            F(I,J) = F(I,J)-PERTRB
  128    CONTINUE
  129 CONTINUE
  130 CONTINUE
C
C     MULTIPLY I-TH EQUATION THROUGH BY  DELTHT**2
C
      DO 132 I=1,M
         W(I) = W(I)*DLTHSQ
         J = IWC+I
         W(J) = W(J)*DLTHSQ
         J = IWB+I
         W(J) = W(J)*DLTHSQ
         DO 131 J=1,N
            F(I,J) = F(I,J)*DLTHSQ
  131    CONTINUE
  132 CONTINUE
      LP = NBDCND
      W(1) = 0.
      W(IWR) = 0.
C
C     CALL GENBUN TO SOLVE THE SYSTEM OF EQUATIONS.
C
      IF (NBDCND .EQ. 0) GO TO 133
      CALL POISTG (LP,N,1,M,W,W(IWB+1),W(IWC+1),IDIMF,F,IERR1,W(IWR+1))
      GO TO 134
  133 CALL GENBUN (LP,N,1,M,W,W(IWB+1),W(IWC+1),IDIMF,F,IERR1,W(IWR+1))
  134 CONTINUE
      W(1) = W(IWR+1)+3*M
      RETURN
      END
