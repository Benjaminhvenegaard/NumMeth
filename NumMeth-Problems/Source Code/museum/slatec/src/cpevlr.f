      SUBROUTINE CPEVLR (N, M, A, X, C)
      REAL A(*),C(*)
C***FIRST EXECUTABLE STATEMENT  CPEVLR
      NP1=N+1
      DO 1 J=1,NP1
            CI=0.0
            CIM1=A(J)
            MINI=MIN(M+1,N+2-J)
            DO 1 I=1,MINI
               IF(J .NE. 1) CI=C(I)
               IF(I .NE. 1) CIM1=C(I-1)
               C(I)=CIM1+X*CI
    1 CONTINUE
      RETURN
      END
