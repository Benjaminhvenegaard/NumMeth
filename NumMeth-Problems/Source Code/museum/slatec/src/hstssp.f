      SUBROUTINE HSTSSP (A, B, M, MBDCND, BDA, BDB, C, D, N, NBDCND,
     +   BDC, BDD, ELMBDA, F, IDIMF, PERTRB, IERROR, W)
C
C
      DIMENSION       F(IDIMF,*) ,BDA(*)     ,BDB(*)     ,BDC(*)     ,
     1                BDD(*)     ,W(*)
C***FIRST EXECUTABLE STATEMENT  HSTSSP
      IERROR = 0
      PI = PIMACH(DUM)
      IF (A.LT.0. .OR. B.GT.PI) IERROR = 1
      IF (A .GE. B) IERROR = 2
      IF (MBDCND.LE.0 .OR. MBDCND.GT.9) IERROR = 3
      IF (C .GE. D) IERROR = 4
      IF (N .LE. 2) IERROR = 5
      IF (NBDCND.LT.0 .OR. NBDCND.GE.5) IERROR = 6
      IF (A.GT.0. .AND. (MBDCND.EQ.5 .OR. MBDCND.EQ.6 .OR. MBDCND.EQ.9))
     1    IERROR = 7
      IF (A.EQ.0. .AND. (MBDCND.EQ.3 .OR. MBDCND.EQ.4 .OR. MBDCND.EQ.8))
     1    IERROR = 8
      IF (B.LT.PI .AND. MBDCND.GE.7) IERROR = 9
      IF (B.EQ.PI .AND. (MBDCND.EQ.2 .OR. MBDCND.EQ.3 .OR. MBDCND.EQ.6))
     1    IERROR = 10
      IF (MBDCND.GE.5 .AND.
     1    (NBDCND.EQ.1 .OR. NBDCND.EQ.2 .OR. NBDCND.EQ.4)) IERROR = 11
      IF (IDIMF .LT. M) IERROR = 12
      IF (M .LE. 2) IERROR = 13
      IF (IERROR .NE. 0) RETURN
      DELTAR = (B-A)/M
      DLRSQ = DELTAR**2
      DELTHT = (D-C)/N
      DLTHSQ = DELTHT**2
      NP = NBDCND+1
      ISW = 1
      JSW = 1
      MB = MBDCND
      IF (ELMBDA .NE. 0.) GO TO 105
      GO TO (101,102,105,103,101,105,101,105,105),MBDCND
  101 IF (A.NE.0. .OR. B.NE.PI) GO TO 105
      MB = 9
      GO TO 104
  102 IF (A .NE. 0.) GO TO 105
      MB = 6
      GO TO 104
  103 IF (B .NE. PI) GO TO 105
      MB = 8
  104 JSW = 2
  105 CONTINUE
C
C     DEFINE A,B,C COEFFICIENTS IN W-ARRAY.
C
      IWB = M
      IWC = IWB+M
      IWR = IWC+M
      IWS = IWR+M
      DO 106 I=1,M
         J = IWR+I
         W(J) = SIN(A+(I-0.5)*DELTAR)
         W(I) = SIN((A+(I-1)*DELTAR))/DLRSQ
  106 CONTINUE
      MM1 = M-1
      DO 107 I=1,MM1
         K = IWC+I
         W(K) = W(I+1)
         J = IWR+I
         K = IWB+I
         W(K) = ELMBDA*W(J)-(W(I)+W(I+1))
  107 CONTINUE
      W(IWR) = SIN(B)/DLRSQ
      W(IWC) = ELMBDA*W(IWS)-(W(M)+W(IWR))
      DO 109 I=1,M
         J = IWR+I
         A1 = W(J)
         DO 108 J=1,N
            F(I,J) = A1*F(I,J)
  108    CONTINUE
  109 CONTINUE
C
C     ENTER BOUNDARY DATA FOR THETA-BOUNDARIES.
C
      GO TO (110,110,112,112,114,114,110,112,114),MB
  110 A1 = 2.*W(1)
      W(IWB+1) = W(IWB+1)-W(1)
      DO 111 J=1,N
         F(1,J) = F(1,J)-A1*BDA(J)
  111 CONTINUE
      GO TO 114
  112 A1 = DELTAR*W(1)
      W(IWB+1) = W(IWB+1)+W(1)
      DO 113 J=1,N
         F(1,J) = F(1,J)+A1*BDA(J)
  113 CONTINUE
  114 GO TO (115,117,117,115,115,117,119,119,119),MB
  115 A1 = 2.*W(IWR)
      W(IWC) = W(IWC)-W(IWR)
      DO 116 J=1,N
         F(M,J) = F(M,J)-A1*BDB(J)
  116 CONTINUE
      GO TO 119
  117 A1 = DELTAR*W(IWR)
      W(IWC) = W(IWC)+W(IWR)
      DO 118 J=1,N
         F(M,J) = F(M,J)-A1*BDB(J)
  118 CONTINUE
C
C     ENTER BOUNDARY DATA FOR PHI-BOUNDARIES.
C
  119 A1 = 2./DLTHSQ
      GO TO (129,120,120,122,122),NP
  120 DO 121 I=1,M
         J = IWR+I
         F(I,1) = F(I,1)-A1*BDC(I)/W(J)
  121 CONTINUE
      GO TO 124
  122 A1 = 1./DELTHT
      DO 123 I=1,M
         J = IWR+I
         F(I,1) = F(I,1)+A1*BDC(I)/W(J)
  123 CONTINUE
  124 A1 = 2./DLTHSQ
      GO TO (129,125,127,127,125),NP
  125 DO 126 I=1,M
         J = IWR+I
         F(I,N) = F(I,N)-A1*BDD(I)/W(J)
  126 CONTINUE
      GO TO 129
  127 A1 = 1./DELTHT
      DO 128 I=1,M
         J = IWR+I
         F(I,N) = F(I,N)-A1*BDD(I)/W(J)
  128 CONTINUE
  129 CONTINUE
C
C     ADJUST RIGHT SIDE OF SINGULAR PROBLEMS TO INSURE EXISTENCE OF A
C     SOLUTION.
C
      PERTRB = 0.
      IF (ELMBDA) 139,131,130
  130 IERROR = 14
      GO TO 139
  131 GO TO (139,139,132,139,139,132,139,132,132),MB
  132 GO TO (133,139,139,133,139),NP
  133 CONTINUE
      ISW = 2
      DO 135 J=1,N
         DO 134 I=1,M
            PERTRB = PERTRB+F(I,J)
  134    CONTINUE
  135 CONTINUE
      A1 = N*(COS(A)-COS(B))/(2.*SIN(0.5*DELTAR))
      PERTRB = PERTRB/A1
      DO 137 I=1,M
         J = IWR+I
         A1 = PERTRB*W(J)
         DO 136 J=1,N
            F(I,J) = F(I,J)-A1
  136    CONTINUE
  137 CONTINUE
      A2 = 0.
      A3 = 0.
      DO 138 J=1,N
         A2 = A2+F(1,J)
         A3 = A3+F(M,J)
  138 CONTINUE
      A2 = A2/W(IWR+1)
      A3 = A3/W(IWS)
  139 CONTINUE
C
C     MULTIPLY I-TH EQUATION THROUGH BY  R(I)*DELTHT**2
C
      DO 141 I=1,M
         J = IWR+I
         A1 = DLTHSQ*W(J)
         W(I) = A1*W(I)
         J = IWC+I
         W(J) = A1*W(J)
         J = IWB+I
         W(J) = A1*W(J)
         DO 140 J=1,N
            F(I,J) = A1*F(I,J)
  140    CONTINUE
  141 CONTINUE
      LP = NBDCND
      W(1) = 0.
      W(IWR) = 0.
C
C     CALL POISTG OR GENBUN TO SOLVE THE SYSTEM OF EQUATIONS.
C
      IF (NBDCND .EQ. 0) GO TO 142
      CALL POISTG (LP,N,1,M,W,W(IWB+1),W(IWC+1),IDIMF,F,IERR1,W(IWR+1))
      GO TO 143
  142 CALL GENBUN (LP,N,1,M,W,W(IWB+1),W(IWC+1),IDIMF,F,IERR1,W(IWR+1))
  143 CONTINUE
      W(1) = W(IWR+1)+3*M
      IF (ISW.NE.2 .OR. JSW.NE.2) GO TO 150
      IF (MB .NE. 8) GO TO 145
      A1 = 0.
      DO 144 J=1,N
         A1 = A1+F(M,J)
  144 CONTINUE
      A1 = (A1-DLRSQ*A3/16.)/N
      IF (NBDCND .EQ. 3) A1 = A1+(BDD(M)-BDC(M))/(D-C)
      A1 = BDB(1)-A1
      GO TO 147
  145 A1 = 0.
      DO 146 J=1,N
         A1 = A1+F(1,J)
  146 CONTINUE
      A1 = (A1-DLRSQ*A2/16.)/N
      IF (NBDCND .EQ. 3) A1 = A1+(BDD(1)-BDC(1))/(D-C)
      A1 = BDA(1)-A1
  147 DO 149 I=1,M
         DO 148 J=1,N
            F(I,J) = F(I,J)+A1
  148    CONTINUE
  149 CONTINUE
  150 CONTINUE
      RETURN
      END
