      SUBROUTINE CH (NM, N, AR, AI, W, MATZ, ZR, ZI, FV1, FV2, FM1,
     +   IERR)
C
      INTEGER I,J,N,NM,IERR,MATZ
      REAL AR(NM,*),AI(NM,*),W(*),ZR(NM,*),ZI(NM,*)
      REAL FV1(*),FV2(*),FM1(2,*)
C
C***FIRST EXECUTABLE STATEMENT  CH
      IF (N .LE. NM) GO TO 10
      IERR = 10 * N
      GO TO 50
C
   10 CALL  HTRIDI(NM,N,AR,AI,W,FV1,FV2,FM1)
      IF (MATZ .NE. 0) GO TO 20
C     .......... FIND EIGENVALUES ONLY ..........
      CALL  TQLRAT(N,W,FV2,IERR)
      GO TO 50
C     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS ..........
   20 DO 40 I = 1, N
C
         DO 30 J = 1, N
            ZR(J,I) = 0.0E0
   30    CONTINUE
C
         ZR(I,I) = 1.0E0
   40 CONTINUE
C
      CALL  TQL2(NM,N,W,FV1,ZR,IERR)
      IF (IERR .NE. 0) GO TO 50
      CALL  HTRIBK(NM,N,AR,AI,FM1,N,ZR,ZI)
   50 RETURN
      END
