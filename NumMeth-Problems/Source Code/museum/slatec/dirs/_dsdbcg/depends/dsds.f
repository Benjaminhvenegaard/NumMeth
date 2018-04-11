      SUBROUTINE DSDS (N, NELT, IA, JA, A, ISYM, DINV)
C     .. Scalar Arguments ..
      INTEGER ISYM, N, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), DINV(N)
      INTEGER IA(NELT), JA(NELT)
C     .. Local Scalars ..
      INTEGER ICOL
C***FIRST EXECUTABLE STATEMENT  DSDS
C
C         Assume the Diagonal elements are the first in each column.
C         This loop should *VECTORIZE*.  If it does not you may have
C         to add a compiler directive.  We do not check for a zero
C         (or near zero) diagonal element since this would interfere
C         with vectorization.  If this makes you nervous put a check
C         in!  It will run much slower.
C
      DO 10 ICOL = 1, N
         DINV(ICOL) = 1.0D0/A(JA(ICOL))
 10   CONTINUE
C
      RETURN
C------------- LAST LINE OF DSDS FOLLOWS ----------------------------
      END
