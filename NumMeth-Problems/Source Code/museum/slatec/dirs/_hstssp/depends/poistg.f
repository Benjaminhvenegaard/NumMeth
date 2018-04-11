      SUBROUTINE POISTG (NPEROD, N, MPEROD, M, A, B, C, IDIMY, Y,
     +   IERROR, W)
C
C
      DIMENSION       Y(IDIMY,*)
      DIMENSION       W(*)       ,B(*)       ,A(*)       ,C(*)
C***FIRST EXECUTABLE STATEMENT  POISTG
      IERROR = 0
      IF (M .LE. 2) IERROR = 1
      IF (N .LE. 2) IERROR = 2
      IF (IDIMY .LT. M) IERROR = 3
      IF (NPEROD.LT.1 .OR. NPEROD.GT.4) IERROR = 4
      IF (MPEROD.LT.0 .OR. MPEROD.GT.1) IERROR = 5
      IF (MPEROD .EQ. 1) GO TO 103
      DO 101 I=1,M
         IF (A(I) .NE. C(1)) GO TO 102
         IF (C(I) .NE. C(1)) GO TO 102
         IF (B(I) .NE. B(1)) GO TO 102
  101 CONTINUE
      GO TO 104
  102 IERROR = 6
      RETURN
  103 IF (A(1).NE.0. .OR. C(M).NE.0.) IERROR = 7
  104 IF (IERROR .NE. 0) RETURN
      IWBA = M+1
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
      NP = NPEROD
      MP = MPEROD+1
      GO TO (110,107),MP
  107 CONTINUE
      GO TO (108,108,108,119),NPEROD
  108 CONTINUE
      CALL POSTG2 (NP,N,M,W(IWBA),W(IWBB),W(IWBC),IDIMY,Y,W,W(IWB2),
     1             W(IWB3),W(IWW1),W(IWW2),W(IWW3),W(IWD),W(IWTCOS),
     2             W(IWP))
      IPSTOR = W(IWW1)
      IREV = 2
      IF (NPEROD .EQ. 4) GO TO 120
  109 CONTINUE
      GO TO (123,129),MP
  110 CONTINUE
C
C     REORDER UNKNOWNS WHEN MP =0
C
      MH = (M+1)/2
      MHM1 = MH-1
      MODD = 1
      IF (MH*2 .EQ. M) MODD = 2
      DO 115 J=1,N
         DO 111 I=1,MHM1
            MHPI = MH+I
            MHMI = MH-I
            W(I) = Y(MHMI,J)-Y(MHPI,J)
            W(MHPI) = Y(MHMI,J)+Y(MHPI,J)
  111    CONTINUE
         W(MH) = 2.*Y(MH,J)
         GO TO (113,112),MODD
  112    W(M) = 2.*Y(M,J)
  113    CONTINUE
         DO 114 I=1,M
            Y(I,J) = W(I)
  114    CONTINUE
  115 CONTINUE
      K = IWBC+MHM1-1
      I = IWBA+MHM1
      W(K) = 0.
      W(I) = 0.
      W(K+1) = 2.*W(K+1)
      GO TO (116,117),MODD
  116 CONTINUE
      K = IWBB+MHM1-1
      W(K) = W(K)-W(I-1)
      W(IWBC-1) = W(IWBC-1)+W(IWBB-1)
      GO TO 118
  117 W(IWBB-1) = W(K+1)
  118 CONTINUE
      GO TO 107
  119 CONTINUE
C
C     REVERSE COLUMNS WHEN NPEROD = 4.
C
      IREV = 1
      NBY2 = N/2
      NP = 2
  120 DO 122 J=1,NBY2
         MSKIP = N+1-J
         DO 121 I=1,M
            A1 = Y(I,J)
            Y(I,J) = Y(I,MSKIP)
            Y(I,MSKIP) = A1
  121    CONTINUE
  122 CONTINUE
      GO TO (108,109),IREV
  123 CONTINUE
      DO 128 J=1,N
         DO 124 I=1,MHM1
            MHMI = MH-I
            MHPI = MH+I
            W(MHMI) = .5*(Y(MHPI,J)+Y(I,J))
            W(MHPI) = .5*(Y(MHPI,J)-Y(I,J))
  124    CONTINUE
         W(MH) = .5*Y(MH,J)
         GO TO (126,125),MODD
  125    W(M) = .5*Y(M,J)
  126    CONTINUE
         DO 127 I=1,M
            Y(I,J) = W(I)
  127    CONTINUE
  128 CONTINUE
  129 CONTINUE
C
C     RETURN STORAGE REQUIREMENTS FOR W ARRAY.
C
      W(1) = IPSTOR+IWP-1
      RETURN
      END
