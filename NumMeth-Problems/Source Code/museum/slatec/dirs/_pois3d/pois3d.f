      SUBROUTINE POIS3D (LPEROD, L, C1, MPEROD, M, C2, NPEROD, N, A, B,
     +   C, LDIMF, MDIMF, F, IERROR, W)
      DIMENSION       A(*)       ,B(*)       ,C(*)       ,
     1                F(LDIMF,MDIMF,*)       ,W(*)       ,SAVE(6)
C***FIRST EXECUTABLE STATEMENT  POIS3D
      LP = LPEROD+1
      MP = MPEROD+1
      NP = NPEROD+1
C
C     CHECK FOR INVALID INPUT.
C
      IERROR = 0
      IF (LP.LT.1 .OR. LP.GT.5) IERROR = 1
      IF (L .LT. 3) IERROR = 2
      IF (MP.LT.1 .OR. MP.GT.5) IERROR = 3
      IF (M .LT. 3) IERROR = 4
      IF (NP.LT.1 .OR. NP.GT.2) IERROR = 5
      IF (N .LT. 3) IERROR = 6
      IF (LDIMF .LT. L) IERROR = 7
      IF (MDIMF .LT. M) IERROR = 8
      IF (NP .NE. 1) GO TO 103
      DO 101 K=1,N
         IF (A(K) .NE. C(1)) GO TO 102
         IF (C(K) .NE. C(1)) GO TO 102
         IF (B(K) .NE. B(1)) GO TO 102
  101 CONTINUE
      GO TO 104
  102 IERROR = 9
  103 IF (NPEROD.EQ.1 .AND. (A(1).NE.0. .OR. C(N).NE.0.)) IERROR = 10
  104 IF (IERROR .NE. 0) GO TO 122
      IWYRT = L+1
      IWT = IWYRT+M
      IWD = IWT+MAX(L,M,N)+1
      IWBB = IWD+N
      IWX = IWBB+N
      IWY = IWX+7*((L+1)/2)+15
      GO TO (105,114),NP
C
C     REORDER UNKNOWNS WHEN NPEROD = 0.
C
  105 NH = (N+1)/2
      NHM1 = NH-1
      NODD = 1
      IF (2*NH .EQ. N) NODD = 2
      DO 111 I=1,L
         DO 110 J=1,M
            DO 106 K=1,NHM1
               NHPK = NH+K
               NHMK = NH-K
               W(K) = F(I,J,NHMK)-F(I,J,NHPK)
               W(NHPK) = F(I,J,NHMK)+F(I,J,NHPK)
  106       CONTINUE
            W(NH) = 2.*F(I,J,NH)
            GO TO (108,107),NODD
  107       W(N) = 2.*F(I,J,N)
  108       DO 109 K=1,N
               F(I,J,K) = W(K)
  109       CONTINUE
  110    CONTINUE
  111 CONTINUE
      SAVE(1) = C(NHM1)
      SAVE(2) = A(NH)
      SAVE(3) = C(NH)
      SAVE(4) = B(NHM1)
      SAVE(5) = B(N)
      SAVE(6) = A(N)
      C(NHM1) = 0.
      A(NH) = 0.
      C(NH) = 2.*C(NH)
      GO TO (112,113),NODD
  112 B(NHM1) = B(NHM1)-A(NH-1)
      B(N) = B(N)+A(N)
      GO TO 114
  113 A(N) = C(NH)
  114 CONTINUE
      CALL POS3D1 (LP,L,MP,M,N,A,B,C,LDIMF,MDIMF,F,W,W(IWYRT),W(IWT),
     1             W(IWD),W(IWX),W(IWY),C1,C2,W(IWBB))
      GO TO (115,122),NP
  115 DO 121 I=1,L
         DO 120 J=1,M
            DO 116 K=1,NHM1
               NHMK = NH-K
               NHPK = NH+K
               W(NHMK) = .5*(F(I,J,NHPK)+F(I,J,K))
               W(NHPK) = .5*(F(I,J,NHPK)-F(I,J,K))
  116       CONTINUE
            W(NH) = .5*F(I,J,NH)
            GO TO (118,117),NODD
  117       W(N) = .5*F(I,J,N)
  118       DO 119 K=1,N
               F(I,J,K) = W(K)
  119       CONTINUE
  120    CONTINUE
  121 CONTINUE
      C(NHM1) = SAVE(1)
      A(NH) = SAVE(2)
      C(NH) = SAVE(3)
      B(NHM1) = SAVE(4)
      B(N) = SAVE(5)
      A(N) = SAVE(6)
  122 CONTINUE
      RETURN
      END
