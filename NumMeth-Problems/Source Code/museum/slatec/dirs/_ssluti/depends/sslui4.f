      SUBROUTINE SSLUI4 (N, B, X, IL, JL, L, DINV, IU, JU, U)
C     .. Scalar Arguments ..
      INTEGER N
C     .. Array Arguments ..
      REAL B(N), DINV(N), L(*), U(*), X(N)
      INTEGER IL(*), IU(*), JL(*), JU(*)
C     .. Local Scalars ..
      INTEGER I, ICOL, IROW, J, JBGN, JEND
C***FIRST EXECUTABLE STATEMENT  SSLUI4
      DO 10 I=1,N
         X(I) = B(I)
 10   CONTINUE
C
C         Solve  U'*Y = X,  storing result in X, U stored by columns.
      DO 80 IROW = 2, N
         JBGN = JU(IROW)
         JEND = JU(IROW+1) - 1
         IF( JBGN.LE.JEND ) THEN
CLLL. OPTION ASSERT (NOHAZARD)
CDIR$ IVDEP
CVD$ ASSOC
CVD$ NODEPCHK
            DO 70 J = JBGN, JEND
               X(IROW) = X(IROW) - U(J)*X(IU(J))
 70         CONTINUE
         ENDIF
 80   CONTINUE
C
C         Solve  D*Z = Y,  storing result in X.
      DO 90 I = 1, N
         X(I) = X(I)*DINV(I)
 90   CONTINUE
C
C         Solve  L'*X = Z, L stored by rows.
      DO 110 ICOL = N, 2, -1
         JBGN = IL(ICOL)
         JEND = IL(ICOL+1) - 1
         IF( JBGN.LE.JEND ) THEN
CLLL. OPTION ASSERT (NOHAZARD)
CDIR$ IVDEP
CVD$ NODEPCHK
            DO 100 J = JBGN, JEND
               X(JL(J)) = X(JL(J)) - L(J)*X(ICOL)
 100        CONTINUE
         ENDIF
 110  CONTINUE
      RETURN
C------------- LAST LINE OF SSLUI4 FOLLOWS ----------------------------
      END
