      SUBROUTINE RSB (NM, N, MB, A, W, MATZ, Z, FV1, FV2, IERR)
C
      INTEGER N,MB,NM,IERR,MATZ
      REAL A(NM,*),W(*),Z(NM,*),FV1(*),FV2(*)
      LOGICAL TF
C
C***FIRST EXECUTABLE STATEMENT  RSB
      IF (N .LE. NM) GO TO 5
      IERR = 10 * N
      GO TO 50
    5 IF (MB .GT. 0) GO TO 10
      IERR = 12 * N
      GO TO 50
   10 IF (MB .LE. N) GO TO 15
      IERR = 12 * N
      GO TO 50
C
   15 IF (MATZ .NE. 0) GO TO 20
C     .......... FIND EIGENVALUES ONLY ..........
      TF = .FALSE.
      CALL  BANDR(NM,N,MB,A,W,FV1,FV2,TF,Z)
      CALL  TQLRAT(N,W,FV2,IERR)
      GO TO 50
C     .......... FIND BOTH EIGENVALUES AND EIGENVECTORS ..........
   20 TF = .TRUE.
      CALL  BANDR(NM,N,MB,A,W,FV1,FV1,TF,Z)
      CALL  TQL2(NM,N,W,FV1,Z,IERR)
   50 RETURN
      END
