      SUBROUTINE DS2LT (N, NELT, IA, JA, A, ISYM, NEL, IEL, JEL, EL)
C     .. Scalar Arguments ..
      INTEGER ISYM, N, NEL, NELT
C     .. Array Arguments ..
      DOUBLE PRECISION A(NELT), EL(NELT)
      INTEGER IA(NELT), IEL(NEL), JA(NELT), JEL(NEL)
C     .. Local Scalars ..
      INTEGER I, ICOL, J, JBGN, JEND
C***FIRST EXECUTABLE STATEMENT  DS2LT
      IF( ISYM.EQ.0 ) THEN
C
C         The matrix is stored non-symmetricly.  Pick out the lower
C         triangle.
C
         NEL = 0
         DO 20 ICOL = 1, N
            JEL(ICOL) = NEL+1
            JBGN = JA(ICOL)
            JEND = JA(ICOL+1)-1
CVD$ NOVECTOR
            DO 10 J = JBGN, JEND
               IF( IA(J).GE.ICOL ) THEN
                  NEL = NEL + 1
                  IEL(NEL) = IA(J)
                  EL(NEL)  = A(J)
               ENDIF
 10         CONTINUE
 20      CONTINUE
         JEL(N+1) = NEL+1
      ELSE
C
C         The matrix is symmetric and only the lower triangle is
C         stored.  Copy it to IEL, JEL, EL.
C
         NEL = NELT
         DO 30 I = 1, NELT
            IEL(I) = IA(I)
            EL(I) = A(I)
 30      CONTINUE
         DO 40 I = 1, N+1
            JEL(I) = JA(I)
 40      CONTINUE
      ENDIF
      RETURN
C------------- LAST LINE OF DS2LT FOLLOWS ----------------------------
      END
