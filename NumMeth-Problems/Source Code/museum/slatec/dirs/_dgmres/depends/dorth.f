      SUBROUTINE DORTH (VNEW, V, HES, N, LL, LDHES, KMP, SNORMW)
C         The following is for optimized compilation on LLNL/LTSS Crays.
CLLL. OPTIMIZE
C     .. Scalar Arguments ..
      DOUBLE PRECISION SNORMW
      INTEGER KMP, LDHES, LL, N
C     .. Array Arguments ..
      DOUBLE PRECISION HES(LDHES,*), V(N,*), VNEW(*)
C     .. Local Scalars ..
      DOUBLE PRECISION ARG, SUMDSQ, TEM, VNRM
      INTEGER I, I0
C     .. External Functions ..
      DOUBLE PRECISION DDOT, DNRM2
      EXTERNAL DDOT, DNRM2
C     .. External Subroutines ..
      EXTERNAL DAXPY
C     .. Intrinsic Functions ..
      INTRINSIC MAX, SQRT
C***FIRST EXECUTABLE STATEMENT  DORTH
C
C         Get norm of unaltered VNEW for later use.
C
      VNRM = DNRM2(N, VNEW, 1)
C   -------------------------------------------------------------------
C         Perform the modified Gram-Schmidt procedure on VNEW =A*V(LL).
C         Scaled inner products give new column of HES.
C         Projections of earlier vectors are subtracted from VNEW.
C   -------------------------------------------------------------------
      I0 = MAX(1,LL-KMP+1)
      DO 10 I = I0,LL
         HES(I,LL) = DDOT(N, V(1,I), 1, VNEW, 1)
         TEM = -HES(I,LL)
         CALL DAXPY(N, TEM, V(1,I), 1, VNEW, 1)
 10   CONTINUE
C   -------------------------------------------------------------------
C         Compute SNORMW = norm of VNEW.  If VNEW is small compared
C         to its input value (in norm), then reorthogonalize VNEW to
C         V(*,1) through V(*,LL).  Correct if relative correction
C         exceeds 1000*(unit roundoff).  Finally, correct SNORMW using
C         the dot products involved.
C   -------------------------------------------------------------------
      SNORMW = DNRM2(N, VNEW, 1)
      IF (VNRM + 0.001D0*SNORMW .NE. VNRM) RETURN
      SUMDSQ = 0
      DO 30 I = I0,LL
         TEM = -DDOT(N, V(1,I), 1, VNEW, 1)
         IF (HES(I,LL) + 0.001D0*TEM .EQ. HES(I,LL)) GO TO 30
         HES(I,LL) = HES(I,LL) - TEM
         CALL DAXPY(N, TEM, V(1,I), 1, VNEW, 1)
         SUMDSQ = SUMDSQ + TEM**2
 30   CONTINUE
      IF (SUMDSQ .EQ. 0.0D0) RETURN
      ARG = MAX(0.0D0,SNORMW**2 - SUMDSQ)
      SNORMW = SQRT(ARG)
C
      RETURN
C------------- LAST LINE OF DORTH FOLLOWS ----------------------------
      END
