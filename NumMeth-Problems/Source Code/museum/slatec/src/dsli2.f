      SUBROUTINE DSLI2 (N, B, X, NEL, IEL, JEL, EL)
C     .. Scalar Arguments ..
      INTEGER N, NEL
C     .. Array Arguments ..
      DOUBLE PRECISION B(N), EL(NEL), X(N)
      INTEGER IEL(NEL), JEL(NEL)
C     .. Local Scalars ..
      INTEGER I, ICOL, J, JBGN, JEND
C***FIRST EXECUTABLE STATEMENT  DSLI2
C
C         Initialize the solution by copying the right hands side
C         into it.
C
      DO 10 I=1,N
         X(I) = B(I)
 10   CONTINUE
C
CVD$ NOCONCUR
      DO 30 ICOL = 1, N
         X(ICOL) = X(ICOL)/EL(JEL(ICOL))
         JBGN = JEL(ICOL) + 1
         JEND = JEL(ICOL+1) - 1
         IF( JBGN.LE.JEND ) THEN
CLLL. OPTION ASSERT (NOHAZARD)
CDIR$ IVDEP
CVD$ NOCONCUR
CVD$ NODEPCHK
            DO 20 J = JBGN, JEND
               X(IEL(J)) = X(IEL(J)) - EL(J)*X(ICOL)
 20         CONTINUE
         ENDIF
 30   CONTINUE
C
      RETURN
C------------- LAST LINE OF DSLI2 FOLLOWS ----------------------------
      END
