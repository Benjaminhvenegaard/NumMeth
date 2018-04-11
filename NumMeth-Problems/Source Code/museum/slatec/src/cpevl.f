      SUBROUTINE CPEVL (N, M, A, Z, C, B, KBD)
C
      COMPLEX A(*),C(*),Z,CI,CIM1,B(*),BI,BIM1,T,ZA,Q
      LOGICAL KBD
      SAVE D1
      DATA D1 /0.0/
      ZA(Q)=CMPLX(ABS(REAL(Q)),ABS(AIMAG(Q)))
C***FIRST EXECUTABLE STATEMENT  CPEVL
      IF (D1 .EQ. 0.0) D1 = REAL(I1MACH(10))**(1-I1MACH(11))
      NP1=N+1
      DO 1 J=1,NP1
         CI=0.0
         CIM1=A(J)
         BI=0.0
         BIM1=0.0
         MINI=MIN(M+1,N+2-J)
            DO 1 I=1,MINI
               IF(J .NE. 1) CI=C(I)
               IF(I .NE. 1) CIM1=C(I-1)
               C(I)=CIM1+Z*CI
               IF(.NOT. KBD) GO TO 1
               IF(J .NE. 1) BI=B(I)
               IF(I .NE. 1) BIM1=B(I-1)
               T=BI+(3.*D1+4.*D1*D1)*ZA(CI)
               R=REAL(ZA(Z)*CMPLX(REAL(T),-AIMAG(T)))
               S=AIMAG(ZA(Z)*T)
               B(I)=(1.+8.*D1)*(BIM1+D1*ZA(CIM1)+CMPLX(R,S))
               IF(J .EQ. 1) B(I)=0.0
    1 CONTINUE
      RETURN
      END
