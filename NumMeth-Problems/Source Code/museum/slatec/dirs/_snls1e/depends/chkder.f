      SUBROUTINE CHKDER (M, N, X, FVEC, FJAC, LDFJAC, XP, FVECP, MODE,
     +   ERR)
      INTEGER M,N,LDFJAC,MODE
      REAL X(*),FVEC(*),FJAC(LDFJAC,*),XP(*),FVECP(*),ERR(*)
      INTEGER I,J
      REAL EPS,EPSF,EPSLOG,EPSMCH,FACTOR,ONE,TEMP,ZERO
      REAL R1MACH
      SAVE FACTOR, ONE, ZERO
C
      DATA FACTOR,ONE,ZERO /1.0E2,1.0E0,0.0E0/
C***FIRST EXECUTABLE STATEMENT  CHKDER
      EPSMCH = R1MACH(4)
C
      EPS = SQRT(EPSMCH)
C
      IF (MODE .EQ. 2) GO TO 20
C
C        MODE = 1.
C
         DO 10 J = 1, N
            TEMP = EPS*ABS(X(J))
            IF (TEMP .EQ. ZERO) TEMP = EPS
            XP(J) = X(J) + TEMP
   10       CONTINUE
         GO TO 70
   20 CONTINUE
C
C        MODE = 2.
C
         EPSF = FACTOR*EPSMCH
         EPSLOG = LOG10(EPS)
         DO 30 I = 1, M
            ERR(I) = ZERO
   30       CONTINUE
         DO 50 J = 1, N
            TEMP = ABS(X(J))
            IF (TEMP .EQ. ZERO) TEMP = ONE
            DO 40 I = 1, M
               ERR(I) = ERR(I) + TEMP*FJAC(I,J)
   40          CONTINUE
   50       CONTINUE
         DO 60 I = 1, M
            TEMP = ONE
            IF (FVEC(I) .NE. ZERO .AND. FVECP(I) .NE. ZERO
     1          .AND. ABS(FVECP(I)-FVEC(I)) .GE. EPSF*ABS(FVEC(I)))
     2         TEMP = EPS*ABS((FVECP(I)-FVEC(I))/EPS-ERR(I))
     3                /(ABS(FVEC(I)) + ABS(FVECP(I)))
            ERR(I) = ONE
            IF (TEMP .GT. EPSMCH .AND. TEMP .LT. EPS)
     1         ERR(I) = (LOG10(TEMP) - EPSLOG)/EPSLOG
            IF (TEMP .GE. EPS) ERR(I) = ZERO
   60       CONTINUE
   70 CONTINUE
C
      RETURN
C
C     LAST CARD OF SUBROUTINE CHKDER.
C
      END
