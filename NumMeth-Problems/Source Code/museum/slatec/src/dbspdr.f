      SUBROUTINE DBSPDR (T, A, N, K, NDERIV, AD)
C
C
      INTEGER I, ID, II, IPKMID, JJ, JM, K, KMID, N, NDERIV
      DOUBLE PRECISION A, AD, DIFF, FKMID, T
C     DIMENSION T(N+K), AD((2*N-NDERIV+1)*NDERIV/2)
      DIMENSION T(*), A(*), AD(*)
C***FIRST EXECUTABLE STATEMENT  DBSPDR
      IF(K.LT.1) GO TO 100
      IF(N.LT.K) GO TO 105
      IF(NDERIV.LT.1 .OR. NDERIV.GT.K) GO TO 110
      DO 10 I=1,N
        AD(I) = A(I)
   10 CONTINUE
      IF (NDERIV.EQ.1) RETURN
      KMID = K
      JJ = N
      JM = 0
      DO 30 ID=2,NDERIV
        KMID = KMID - 1
        FKMID = KMID
        II = 1
        DO 20 I=ID,N
          IPKMID = I + KMID
          DIFF = T(IPKMID) - T(I)
          IF (DIFF.NE.0.0D0) AD(II+JJ) = (AD(II+JM+1)-AD(II+JM))/
     1     DIFF*FKMID
          II = II + 1
   20   CONTINUE
        JM = JJ
        JJ = JJ + N - ID + 1
   30 CONTINUE
      RETURN
C
C
  100 CONTINUE
      CALL XERMSG ('SLATEC', 'DBSPDR', 'K DOES NOT SATISFY K.GE.1', 2,
     +   1)
      RETURN
  105 CONTINUE
      CALL XERMSG ('SLATEC', 'DBSPDR', 'N DOES NOT SATISFY N.GE.K', 2,
     +   1)
      RETURN
  110 CONTINUE
      CALL XERMSG ('SLATEC', 'DBSPDR',
     +   'NDERIV DOES NOT SATISFY 1.LE.NDERIV.LE.K', 2, 1)
      RETURN
      END
