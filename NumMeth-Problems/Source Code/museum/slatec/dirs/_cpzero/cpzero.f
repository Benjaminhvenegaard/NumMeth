      SUBROUTINE CPZERO (IN, A, R, T, IFLG, S)
C
      REAL  S(*)
      COMPLEX R(*),T(*),A(*),PN,TEMP
C***FIRST EXECUTABLE STATEMENT  CPZERO
      IF( IN .LE. 0 .OR. ABS(A(1)) .EQ. 0.0 ) GO TO 30
C
C       CHECK FOR EASILY OBTAINED ZEROS
C
      N=IN
      N1=N+1
      IF(IFLG .NE. 0) GO TO 14
    1 N1=N+1
      IF(N .GT. 1) GO TO 2
         R(1)=-A(2)/A(1)
         S(1)=0.0
         RETURN
    2 IF( ABS(A(N1)) .NE. 0.0 ) GO TO 3
         R(N)=0.0
         S(N)=0.0
         N=N-1
         GO TO 1
C
C          IF INITIAL ESTIMATES FOR ZEROS NOT GIVEN, FIND SOME
C
    3 TEMP=-A(2)/(A(1)*N)
      CALL CPEVL(N,N,A,TEMP,T,T,.FALSE.)
      IMAX=N+2
      T(N1)=ABS(T(N1))
      DO 6 I=2,N1
         T(N+I)=-ABS(T(N+2-I))
         IF(REAL(T(N+I)) .LT. REAL(T(IMAX))) IMAX=N+I
    6 CONTINUE
      X=(-REAL(T(IMAX))/REAL(T(N1)))**(1./(IMAX-N1))
    7 X=2.*X
         CALL CPEVL(N,0,T(N1),CMPLX(X,0.0),PN,PN,.FALSE.)
      IF (REAL(PN).LT.0.) GO TO 7
      U=.5*X
      V=X
   10 X=.5*(U+V)
         CALL CPEVL(N,0,T(N1),CMPLX(X,0.0),PN,PN,.FALSE.)
         IF (REAL(PN).GT.0.) V=X
         IF (REAL(PN).LE.0.) U=X
         IF((V-U) .GT. .001*(1.+V)) GO TO 10
      DO 13 I=1,N
         U=(3.14159265/N)*(2*I-1.5)
   13    R(I)=MAX(X,.001*ABS(TEMP))*CMPLX(COS(U),SIN(U))+TEMP
C
C          MAIN ITERATION LOOP STARTS HERE
C
   14 NR=0
      NMAX=25*N
      DO 19 NIT=1,NMAX
         DO 18 I=1,N
            IF(NIT .NE. 1 .AND. ABS(T(I)) .EQ. 0.) GO TO 18
               CALL CPEVL(N,0,A,R(I),PN,TEMP,.TRUE.)
               IF(ABS(REAL(PN))+ABS(AIMAG(PN)) .GT. REAL(TEMP)+
     1              AIMAG(TEMP)) GO TO 16
                  T(I)=0.0
                  NR=NR+1
                  GO TO 18
   16          TEMP=A(1)
               DO 17 J=1,N
   17             IF(J .NE. I) TEMP=TEMP*(R(I)-R(J))
               T(I)=PN/TEMP
   18    CONTINUE
         DO 15 I=1,N
   15       R(I)=R(I)-T(I)
         IF(NR .EQ. N) GO TO 21
   19 CONTINUE
      GO TO 26
C
C          CALCULATE ERROR BOUNDS FOR ZEROS
C
   21 DO 25 NR=1,N
         CALL CPEVL(N,N,A,R(NR),T,T(N+2),.TRUE.)
         X=ABS(CMPLX(ABS(REAL(T(1))),ABS(AIMAG(T(1))))+T(N+2))
         S(NR)=0.0
         DO 23 I=1,N
            X=X*REAL(N1-I)/I
            TEMP=CMPLX(MAX(ABS(REAL(T(I+1)))-REAL(T(N1+I)),0.0),
     1           MAX(ABS(AIMAG(T(I+1)))-AIMAG(T(N1+I)),0.0))
   23       S(NR)=MAX(S(NR),(ABS(TEMP)/X)**(1./I))
   25    S(NR)=1./S(NR)
      RETURN
C        ERROR EXITS
   26 IFLG=2
      RETURN
   30 IFLG=1
      RETURN
      END