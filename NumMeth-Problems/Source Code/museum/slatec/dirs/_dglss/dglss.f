      SUBROUTINE DGLSS (A, MDA, M, N, B, MDB, NB, RNORM, WORK, LW,
     +   IWORK, LIW, INFO)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(MDA,*),B(MDB,*),RNORM(*),WORK(*)
      INTEGER IWORK(*)
C
C***FIRST EXECUTABLE STATEMENT  DGLSS
      RE=0.D0
      AE=0.D0
      KEY=0
      MODE=2
      NP=0
C
C     IF M.GE.N CALL DLLSIA
C     IF M.LT.N CALL DULSIA
C
      IF(M.LT.N) GO TO 10
      CALL DLLSIA(A,MDA,M,N,B,MDB,NB,RE,AE,KEY,MODE,NP,
     1            KRANK,KSURE,RNORM,WORK,LW,IWORK,LIW,INFO)
      IF(INFO.EQ.-1) RETURN
      INFO=N-KRANK
      RETURN
   10 CALL DULSIA(A,MDA,M,N,B,MDB,NB,RE,AE,KEY,MODE,NP,
     1            KRANK,KSURE,RNORM,WORK,LW,IWORK,LIW,INFO)
      IF(INFO.EQ.-1) RETURN
      INFO=M-KRANK
      RETURN
      END
