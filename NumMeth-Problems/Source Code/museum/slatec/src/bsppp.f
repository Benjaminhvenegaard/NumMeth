      SUBROUTINE BSPPP (T, A, N, K, LDC, C, XI, LXI, WORK)
C
      INTEGER ILEFT, INEV, K, LDC, LXI, N, NK
      REAL A, C, T, WORK, XI
C     DIMENSION T(N+K),XI(LXI+1),C(LDC,*)
C     HERE, * = THE FINAL VALUE OF THE OUTPUT PARAMETER LXI.
      DIMENSION T(*), A(*), WORK(*), XI(*), C(LDC,*)
C***FIRST EXECUTABLE STATEMENT  BSPPP
      IF(K.LT.1) GO TO 100
      IF(N.LT.K) GO TO 105
      IF(LDC.LT.K) GO TO 110
      CALL BSPDR(T, A, N, K, K, WORK)
      LXI = 0
      XI(1) = T(K)
      INEV = 1
      NK = N*K + 1
      DO 10 ILEFT=K,N
        IF (T(ILEFT+1).EQ.T(ILEFT)) GO TO 10
        LXI = LXI + 1
        XI(LXI+1) = T(ILEFT+1)
        CALL BSPEV(T,WORK(1),N,K, K,XI(LXI),INEV,C(1,LXI),WORK(NK))
   10 CONTINUE
      RETURN
  100 CONTINUE
      CALL XERMSG ('SLATEC', 'BSPPP', 'K DOES NOT SATISFY K.GE.1', 2,
     +   1)
      RETURN
  105 CONTINUE
      CALL XERMSG ('SLATEC', 'BSPPP', 'N DOES NOT SATISFY N.GE.K', 2,
     +   1)
      RETURN
  110 CONTINUE
      CALL XERMSG ('SLATEC', 'BSPPP', 'LDC DOES NOT SATISFY LDC.GE.K',
     +   2, 1)
      RETURN
      END
