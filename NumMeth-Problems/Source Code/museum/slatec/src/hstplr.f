      SUBROUTINE HSTPLR (A, B, M, MBDCND, BDA, BDB, C, D, N, NBDCND,
     +   BDC, BDD, ELMBDA, F, IDIMF, PERTRB, IERROR, W)
C
C
      DIMENSION       F(IDIMF,*)
      DIMENSION       BDA(*)     ,BDB(*)     ,BDC(*)     ,BDD(*)     ,
     1                W(*)
C***FIRST EXECUTABLE STATEMENT  HSTPLR
      IERROR = 0
      IF (A .LT. 0.) IERROR = 1
      IF (A .GE. B) IERROR = 2
      IF (MBDCND.LE.0 .OR. MBDCND.GE.7) IERROR = 3
      IF (C .GE. D) IERROR = 4
      IF (N .LE. 2) IERROR = 5
      IF (NBDCND.LT.0 .OR. NBDCND.GE.5) IERROR = 6
      IF (A.EQ.0. .AND. (MBDCND.EQ.3 .OR. MBDCND.EQ.4)) IERROR = 7
      IF (A.GT.0. .AND. MBDCND.GE.5) IERROR = 8
      IF (MBDCND.GE.5 .AND. NBDCND.NE.0 .AND. NBDCND.NE.3) IERROR = 9
      IF (IDIMF .LT. M) IERROR = 10
      IF (M .LE. 2) IERROR = 12
      IF (IERROR .NE. 0) RETURN
      DELTAR = (B-A)/M
      DLRSQ = DELTAR**2
      DELTHT = (D-C)/N
      DLTHSQ = DELTHT**2
      NP = NBDCND+1
      ISW = 1
      MB = MBDCND
      IF (A.EQ.0. .AND. MBDCND.EQ.2) MB = 6
C
C     DEFINE A,B,C COEFFICIENTS IN W-ARRAY.
C
      IWB = M
      IWC = IWB+M
      IWR = IWC+M
      DO 101 I=1,M
         J = IWR+I
         W(J) = A+(I-0.5)*DELTAR
         W(I) = (A+(I-1)*DELTAR)/DLRSQ
         K = IWC+I
         W(K) = (A+I*DELTAR)/DLRSQ
         K = IWB+I
         W(K) = (ELMBDA-2./DLRSQ)*W(J)
  101 CONTINUE
      DO 103 I=1,M
         J = IWR+I
         A1 = W(J)
         DO 102 J=1,N
            F(I,J) = A1*F(I,J)
  102    CONTINUE
  103 CONTINUE
C
C     ENTER BOUNDARY DATA FOR R-BOUNDARIES.
C
      GO TO (104,104,106,106,108,108),MB
  104 A1 = 2.*W(1)
      W(IWB+1) = W(IWB+1)-W(1)
      DO 105 J=1,N
         F(1,J) = F(1,J)-A1*BDA(J)
  105 CONTINUE
      GO TO 108
  106 A1 = DELTAR*W(1)
      W(IWB+1) = W(IWB+1)+W(1)
      DO 107 J=1,N
         F(1,J) = F(1,J)+A1*BDA(J)
  107 CONTINUE
  108 GO TO (109,111,111,109,109,111),MB
  109 A1 = 2.*W(IWR)
      W(IWC) = W(IWC)-W(IWR)
      DO 110 J=1,N
         F(M,J) = F(M,J)-A1*BDB(J)
  110 CONTINUE
      GO TO 113
  111 A1 = DELTAR*W(IWR)
      W(IWC) = W(IWC)+W(IWR)
      DO 112 J=1,N
         F(M,J) = F(M,J)-A1*BDB(J)
  112 CONTINUE
C
C     ENTER BOUNDARY DATA FOR THETA-BOUNDARIES.
C
  113 A1 = 2./DLTHSQ
      GO TO (123,114,114,116,116),NP
  114 DO 115 I=1,M
         J = IWR+I
         F(I,1) = F(I,1)-A1*BDC(I)/W(J)
  115 CONTINUE
      GO TO 118
  116 A1 = 1./DELTHT
      DO 117 I=1,M
         J = IWR+I
         F(I,1) = F(I,1)+A1*BDC(I)/W(J)
  117 CONTINUE
  118 A1 = 2./DLTHSQ
      GO TO (123,119,121,121,119),NP
  119 DO 120 I=1,M
         J = IWR+I
         F(I,N) = F(I,N)-A1*BDD(I)/W(J)
  120 CONTINUE
      GO TO 123
  121 A1 = 1./DELTHT
      DO 122 I=1,M
         J = IWR+I
         F(I,N) = F(I,N)-A1*BDD(I)/W(J)
  122 CONTINUE
  123 CONTINUE
C
C     ADJUST RIGHT SIDE OF SINGULAR PROBLEMS TO INSURE EXISTENCE OF A
C     SOLUTION.
C
      PERTRB = 0.
      IF (ELMBDA) 133,125,124
  124 IERROR = 11
      GO TO 133
  125 GO TO (133,133,126,133,133,126),MB
  126 GO TO (127,133,133,127,133),NP
  127 CONTINUE
      ISW = 2
      DO 129 J=1,N
         DO 128 I=1,M
            PERTRB = PERTRB+F(I,J)
  128    CONTINUE
  129 CONTINUE
      PERTRB = PERTRB/(M*N*0.5*(A+B))
      DO 131 I=1,M
         J = IWR+I
         A1 = PERTRB*W(J)
         DO 130 J=1,N
            F(I,J) = F(I,J)-A1
  130    CONTINUE
  131 CONTINUE
      A2 = 0.
      DO 132 J=1,N
         A2 = A2+F(1,J)
  132 CONTINUE
      A2 = A2/W(IWR+1)
  133 CONTINUE
C
C     MULTIPLY I-TH EQUATION THROUGH BY  R(I)*DELTHT**2
C
      DO 135 I=1,M
         J = IWR+I
         A1 = DLTHSQ*W(J)
         W(I) = A1*W(I)
         J = IWC+I
         W(J) = A1*W(J)
         J = IWB+I
         W(J) = A1*W(J)
         DO 134 J=1,N
            F(I,J) = A1*F(I,J)
  134    CONTINUE
  135 CONTINUE
      LP = NBDCND
      W(1) = 0.
      W(IWR) = 0.
C
C     CALL POISTG OR GENBUN TO SOLVE THE SYSTEM OF EQUATIONS.
C
      IF (LP .EQ. 0) GO TO 136
      CALL POISTG (LP,N,1,M,W,W(IWB+1),W(IWC+1),IDIMF,F,IERR1,W(IWR+1))
      GO TO 137
  136 CALL GENBUN (LP,N,1,M,W,W(IWB+1),W(IWC+1),IDIMF,F,IERR1,W(IWR+1))
  137 CONTINUE
      W(1) = W(IWR+1)+3*M
      IF (A.NE.0. .OR. MBDCND.NE.2 .OR. ISW.NE.2) GO TO 141
      A1 = 0.
      DO 138 J=1,N
         A1 = A1+F(1,J)
  138 CONTINUE
      A1 = (A1-DLRSQ*A2/16.)/N
      IF (NBDCND .EQ. 3) A1 = A1+(BDD(1)-BDC(1))/(D-C)
      A1 = BDA(1)-A1
      DO 140 I=1,M
         DO 139 J=1,N
            F(I,J) = F(I,J)+A1
  139    CONTINUE
  140 CONTINUE
  141 CONTINUE
      RETURN
      END
