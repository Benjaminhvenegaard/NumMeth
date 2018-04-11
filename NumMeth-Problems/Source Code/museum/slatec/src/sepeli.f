      SUBROUTINE SEPELI (INTL, IORDER, A, B, M, MBDCND, BDA, ALPHA, BDB,
     +   BETA, C, D, N, NBDCND, BDC, GAMA, BDD, XNU, COFX, COFY, GRHS,
     +   USOL, IDMN, W, PERTRB, IERROR)
C
      DIMENSION       GRHS(IDMN,*)           ,USOL(IDMN,*)
      DIMENSION       BDA(*)     ,BDB(*)     ,BDC(*)     ,BDD(*)     ,
     1                W(*)
      EXTERNAL        COFX       ,COFY
C***FIRST EXECUTABLE STATEMENT  SEPELI
      CALL CHKPRM (INTL,IORDER,A,B,M,MBDCND,C,D,N,NBDCND,COFX,COFY,
     1             IDMN,IERROR)
      IF (IERROR .NE. 0) RETURN
C
C     COMPUTE MINIMUM WORK SPACE AND CHECK WORK SPACE LENGTH INPUT
C
      L = N+1
      IF (NBDCND .EQ. 0) L = N
      LOGB2N = INT(LOG(L+0.5)/LOG(2.0))+1
      LL = 2**(LOGB2N+1)
      K = M+1
      L = N+1
      LENGTH = (LOGB2N-2)*LL+LOGB2N+MAX(2*L,6*K)+5
      IF (NBDCND .EQ. 0) LENGTH = LENGTH+2*L
      IERROR = 11
      LINPUT = INT(W(1)+0.5)
      LOUTPT = LENGTH+6*(K+L)+1
      W(1) = LOUTPT
      IF (LOUTPT .GT. LINPUT) RETURN
      IERROR = 0
C
C     SET WORK SPACE INDICES
C
      I1 = LENGTH+2
      I2 = I1+L
      I3 = I2+L
      I4 = I3+L
      I5 = I4+L
      I6 = I5+L
      I7 = I6+L
      I8 = I7+K
      I9 = I8+K
      I10 = I9+K
      I11 = I10+K
      I12 = I11+K
      I13 = 2
      CALL SPELIP (INTL,IORDER,A,B,M,MBDCND,BDA,ALPHA,BDB,BETA,C,D,N,
     1             NBDCND,BDC,GAMA,BDD,XNU,COFX,COFY,W(I1),W(I2),W(I3),
     2             W(I4),W(I5),W(I6),W(I7),W(I8),W(I9),W(I10),W(I11),
     3             W(I12),GRHS,USOL,IDMN,W(I13),PERTRB,IERROR)
      RETURN
      END
