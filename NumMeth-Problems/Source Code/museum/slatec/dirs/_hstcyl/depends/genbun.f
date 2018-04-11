      SUBROUTINE GENBUN (NPEROD, N, MPEROD, M, A, B, C, IDIMY, Y,
     +   IERROR, W)
C
C
      DIMENSION       Y(IDIMY,*)
      DIMENSION       W(*)       ,B(*)       ,A(*)       ,C(*)
C***FIRST EXECUTABLE STATEMENT  GENBUN
      IERROR = 0
      IF (M .LE. 2) IERROR = 1
      IF (N .LE. 2) IERROR = 2
      IF (IDIMY .LT. M) IERROR = 3
      IF (NPEROD.LT.0 .OR. NPEROD.GT.4) IERROR = 4
      IF (MPEROD.LT.0 .OR. MPEROD.GT.1) IERROR = 5
      IF (MPEROD .EQ. 1) GO TO 102
      DO 101 I=2,M
         IF (A(I) .NE. C(1)) GO TO 103
         IF (C(I) .NE. C(1)) GO TO 103
         IF (B(I) .NE. B(1)) GO TO 103
  101 CONTINUE
      GO TO 104
  102 IF (A(1).NE.0. .OR. C(M).NE.0.) IERROR = 7
      GO TO 104
  103 IERROR = 6
  104 IF (IERROR .NE. 0) RETURN
      MP1 = M+1
      IWBA = MP1
      IWBB = IWBA+M
      IWBC = IWBB+M
      IWB2 = IWBC+M
      IWB3 = IWB2+M
      IWW1 = IWB3+M
      IWW2 = IWW1+M
      IWW3 = IWW2+M
      IWD = IWW3+M
      IWTCOS = IWD+M
      IWP = IWTCOS+4*N
      DO 106 I=1,M
         K = IWBA+I-1
         W(K) = -A(I)
         K = IWBC+I-1
         W(K) = -C(I)
         K = IWBB+I-1
         W(K) = 2.-B(I)
         DO 105 J=1,N
            Y(I,J) = -Y(I,J)
  105    CONTINUE
  106 CONTINUE
      MP = MPEROD+1
      NP = NPEROD+1
      GO TO (114,107),MP
  107 GO TO (108,109,110,111,123),NP
  108 CALL POISP2 (M,N,W(IWBA),W(IWBB),W(IWBC),Y,IDIMY,W,W(IWB2),
     1             W(IWB3),W(IWW1),W(IWW2),W(IWW3),W(IWD),W(IWTCOS),
     2             W(IWP))
      GO TO 112
  109 CALL POISD2 (M,N,1,W(IWBA),W(IWBB),W(IWBC),Y,IDIMY,W,W(IWW1),
     1             W(IWD),W(IWTCOS),W(IWP))
      GO TO 112
  110 CALL POISN2 (M,N,1,2,W(IWBA),W(IWBB),W(IWBC),Y,IDIMY,W,W(IWB2),
     1             W(IWB3),W(IWW1),W(IWW2),W(IWW3),W(IWD),W(IWTCOS),
     2             W(IWP))
      GO TO 112
  111 CALL POISN2 (M,N,1,1,W(IWBA),W(IWBB),W(IWBC),Y,IDIMY,W,W(IWB2),
     1             W(IWB3),W(IWW1),W(IWW2),W(IWW3),W(IWD),W(IWTCOS),
     2             W(IWP))
  112 IPSTOR = W(IWW1)
      IREV = 2
      IF (NPEROD .EQ. 4) GO TO 124
  113 GO TO (127,133),MP
  114 CONTINUE
C
C     REORDER UNKNOWNS WHEN MP =0
C
      MH = (M+1)/2
      MHM1 = MH-1
      MODD = 1
      IF (MH*2 .EQ. M) MODD = 2
      DO 119 J=1,N
         DO 115 I=1,MHM1
            MHPI = MH+I
            MHMI = MH-I
            W(I) = Y(MHMI,J)-Y(MHPI,J)
            W(MHPI) = Y(MHMI,J)+Y(MHPI,J)
  115    CONTINUE
         W(MH) = 2.*Y(MH,J)
         GO TO (117,116),MODD
  116    W(M) = 2.*Y(M,J)
  117    CONTINUE
         DO 118 I=1,M
            Y(I,J) = W(I)
  118    CONTINUE
  119 CONTINUE
      K = IWBC+MHM1-1
      I = IWBA+MHM1
      W(K) = 0.
      W(I) = 0.
      W(K+1) = 2.*W(K+1)
      GO TO (120,121),MODD
  120 CONTINUE
      K = IWBB+MHM1-1
      W(K) = W(K)-W(I-1)
      W(IWBC-1) = W(IWBC-1)+W(IWBB-1)
      GO TO 122
  121 W(IWBB-1) = W(K+1)
  122 CONTINUE
      GO TO 107
C
C     REVERSE COLUMNS WHEN NPEROD = 4.
C
  123 IREV = 1
      NBY2 = N/2
  124 DO 126 J=1,NBY2
         MSKIP = N+1-J
         DO 125 I=1,M
            A1 = Y(I,J)
            Y(I,J) = Y(I,MSKIP)
            Y(I,MSKIP) = A1
  125    CONTINUE
  126 CONTINUE
      GO TO (110,113),IREV
  127 CONTINUE
      DO 132 J=1,N
         DO 128 I=1,MHM1
            MHMI = MH-I
            MHPI = MH+I
            W(MHMI) = .5*(Y(MHPI,J)+Y(I,J))
            W(MHPI) = .5*(Y(MHPI,J)-Y(I,J))
  128    CONTINUE
         W(MH) = .5*Y(MH,J)
         GO TO (130,129),MODD
  129    W(M) = .5*Y(M,J)
  130    CONTINUE
         DO 131 I=1,M
            Y(I,J) = W(I)
  131    CONTINUE
  132 CONTINUE
  133 CONTINUE
C
C     RETURN STORAGE REQUIREMENTS FOR W ARRAY.
C
      W(1) = IPSTOR+IWP-1
      RETURN
      END
