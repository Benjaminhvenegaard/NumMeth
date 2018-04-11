      SUBROUTINE DRLCAL (N, KMP, LL, MAXL, V, Q, RL, SNORMW, PROD,
     +   R0NRM)
C         The following is for optimized compilation on LLNL/LTSS Crays.
CLLL. OPTIMIZE
C     .. Scalar Arguments ..
      DOUBLE PRECISION PROD, R0NRM, SNORMW
      INTEGER KMP, LL, MAXL, N
C     .. Array Arguments ..
      DOUBLE PRECISION Q(*), RL(N), V(N,*)
C     .. Local Scalars ..
      DOUBLE PRECISION C, S, TEM
      INTEGER I, I2, IP1, K, LLM1, LLP1
C     .. External Subroutines ..
      EXTERNAL DCOPY, DSCAL
C***FIRST EXECUTABLE STATEMENT  DRLCAL
      IF (KMP .EQ. MAXL) THEN
C
C         calculate RL.  Start by copying V(*,1) into RL.
C
         CALL DCOPY(N, V(1,1), 1, RL, 1)
         LLM1 = LL - 1
         DO 20 I = 1,LLM1
            IP1 = I + 1
            I2 = I*2
            S = Q(I2)
            C = Q(I2-1)
            DO 10 K = 1,N
               RL(K) = S*RL(K) + C*V(K,IP1)
 10         CONTINUE
 20      CONTINUE
         S = Q(2*LL)
         C = Q(2*LL-1)/SNORMW
         LLP1 = LL + 1
         DO 30 K = 1,N
            RL(K) = S*RL(K) + C*V(K,LLP1)
 30      CONTINUE
      ENDIF
C
C         When KMP < MAXL, RL vector already partially calculated.
C         Scale RL by R0NRM*PROD to obtain the residual RL.
C
      TEM = R0NRM*PROD
      CALL DSCAL(N, TEM, RL, 1)
      RETURN
C------------- LAST LINE OF DRLCAL FOLLOWS ----------------------------
      END
