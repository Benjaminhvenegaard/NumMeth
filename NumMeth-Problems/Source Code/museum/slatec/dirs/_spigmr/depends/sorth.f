      SUBROUTINE SORTH (VNEW, V, HES, N, LL, LDHES, KMP, SNORMW)
C         The following is for optimized compilation on LLNL/LTSS Crays.
CLLL. OPTIMIZE
C     .. Scalar Arguments ..
      REAL SNORMW
      INTEGER KMP, LDHES, LL, N
C     .. Array Arguments ..
      REAL HES(LDHES,*), V(N,*), VNEW(*)
C     .. Local Scalars ..
      REAL ARG, SUMDSQ, TEM, VNRM
      INTEGER I, I0
C     .. External Functions ..
      REAL SDOT, SNRM2
      EXTERNAL SDOT, SNRM2
C     .. External Subroutines ..
      EXTERNAL SAXPY
C     .. Intrinsic Functions ..
      INTRINSIC MAX, SQRT
C***FIRST EXECUTABLE STATEMENT  SORTH
C
C         Get norm of unaltered VNEW for later use.
C
      VNRM = SNRM2(N, VNEW, 1)
C   -------------------------------------------------------------------
C         Perform the modified Gram-Schmidt procedure on VNEW =A*V(LL).
C         Scaled inner products give new column of HES.
C         Projections of earlier vectors are subtracted from VNEW.
C   -------------------------------------------------------------------
      I0 = MAX(1,LL-KMP+1)
      DO 10 I = I0,LL
         HES(I,LL) = SDOT(N, V(1,I), 1, VNEW, 1)
         TEM = -HES(I,LL)
         CALL SAXPY(N, TEM, V(1,I), 1, VNEW, 1)
 10   CONTINUE
C   -------------------------------------------------------------------
C         Compute SNORMW = norm of VNEW.  If VNEW is small compared
C         to its input value (in norm), then reorthogonalize VNEW to
C         V(*,1) through V(*,LL).  Correct if relative correction
C         exceeds 1000*(unit roundoff).  Finally, correct SNORMW using
C         the dot products involved.
C   -------------------------------------------------------------------
      SNORMW = SNRM2(N, VNEW, 1)
      IF (VNRM + 0.001E0*SNORMW .NE. VNRM) RETURN
      SUMDSQ = 0
      DO 30 I = I0,LL
         TEM = -SDOT(N, V(1,I), 1, VNEW, 1)
         IF (HES(I,LL) + 0.001E0*TEM .EQ. HES(I,LL)) GO TO 30
         HES(I,LL) = HES(I,LL) - TEM
         CALL SAXPY(N, TEM, V(1,I), 1, VNEW, 1)
         SUMDSQ = SUMDSQ + TEM**2
 30   CONTINUE
      IF (SUMDSQ .EQ. 0.0E0) RETURN
      ARG = MAX(0.0E0,SNORMW**2 - SUMDSQ)
      SNORMW = SQRT(ARG)
C
      RETURN
C------------- LAST LINE OF SORTH FOLLOWS ----------------------------
      END
