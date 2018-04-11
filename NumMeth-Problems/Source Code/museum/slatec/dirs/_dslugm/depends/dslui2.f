      SUBROUTINE DSLUI2 (N, B, X, IL, JL, L, DINV, IU, JU, U)
C     .. Scalar Arguments ..
      INTEGER N
C     .. Array Arguments ..
      DOUBLE PRECISION B(N), DINV(N), L(*), U(*), X(N)
      INTEGER IL(*), IU(*), JL(*), JU(*)
C     .. Local Scalars ..
      INTEGER I, ICOL, IROW, J, JBGN, JEND
C***FIRST EXECUTABLE STATEMENT  DSLUI2
C
C         Solve  L*Y = B,  storing result in X, L stored by rows.
C
      DO 10 I = 1, N
         X(I) = B(I)
 10   CONTINUE
      DO 30 IROW = 2, N
         JBGN = IL(IROW)
         JEND = IL(IROW+1)-1
         IF( JBGN.LE.JEND ) THEN
CLLL. OPTION ASSERT (NOHAZARD)
CDIR$ IVDEP
CVD$ ASSOC
CVD$ NODEPCHK
            DO 20 J = JBGN, JEND
               X(IROW) = X(IROW) - L(J)*X(JL(J))
 20         CONTINUE
         ENDIF
 30   CONTINUE
C
C         Solve  D*Z = Y,  storing result in X.
      DO 40 I=1,N
         X(I) = X(I)*DINV(I)
 40   CONTINUE
C
C         Solve  U*X = Z, U stored by columns.
      DO 60 ICOL = N, 2, -1
         JBGN = JU(ICOL)
         JEND = JU(ICOL+1)-1
         IF( JBGN.LE.JEND ) THEN
CLLL. OPTION ASSERT (NOHAZARD)
CDIR$ IVDEP
CVD$ NODEPCHK
            DO 50 J = JBGN, JEND
               X(IU(J)) = X(IU(J)) - U(J)*X(ICOL)
 50         CONTINUE
         ENDIF
 60   CONTINUE
C
      RETURN
C------------- LAST LINE OF DSLUI2 FOLLOWS ----------------------------
      END
