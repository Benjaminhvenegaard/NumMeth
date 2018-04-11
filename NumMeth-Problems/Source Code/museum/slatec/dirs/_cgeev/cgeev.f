      SUBROUTINE CGEEV (A, LDA, N, E, V, LDV, WORK, JOB, INFO)
      INTEGER I,IHI,ILO,INFO,J,K,L,LDA,LDV,MDIM,N
      REAL A(*),E(*),WORK(*),V(*)
C***FIRST EXECUTABLE STATEMENT  CGEEV
      IF (N .GT. LDA) CALL XERMSG ('SLATEC', 'CGEEV', 'N .GT. LDA.', 1,
     +   1)
      IF(N .GT. LDA) RETURN
      IF (N .LT. 1) CALL XERMSG ('SLATEC', 'CGEEV', 'N .LT. 1', 2, 1)
      IF(N .LT. 1) RETURN
      IF(N .EQ. 1 .AND. JOB .EQ. 0) GO TO 35
      MDIM = 2 * LDA
      IF(JOB .EQ. 0) GO TO 5
      IF (N .GT. LDV) CALL XERMSG ('SLATEC', 'CGEEV',
     +   'JOB .NE. 0 AND N .GT. LDV.', 3, 1)
      IF(N .GT. LDV) RETURN
      IF(N .EQ. 1) GO TO 35
C
C       REARRANGE A IF NECESSARY WHEN LDA.GT.LDV AND JOB .NE.0
C
      MDIM = MIN(MDIM,2 * LDV)
      IF (LDA .LT. LDV) CALL XERMSG ('SLATEC', 'CGEEV',
     +   'LDA.LT.LDV,  ELEMENTS OF V OTHER THAN THE N BY N OUTPUT ' //
     +   'ELEMENTS HAVE BEEN CHANGED.', 5, 0)
      IF(LDA.LE.LDV) GO TO 5
      CALL XERMSG ('SLATEC', 'CGEEV',
     +   'LDA.GT.LDV, ELEMENTS OF A OTHER THAN THE N BY N INPUT ' //
     +   'ELEMENTS HAVE BEEN CHANGED.', 4, 0)
      L = N - 1
      DO 4 J=1,L
          I = 2 * N
         M = 1+J*2*LDV
         K = 1+J*2*LDA
         CALL SCOPY(I,A(K),1,A(M),1)
    4 CONTINUE
    5 CONTINUE
C
C     SEPARATE REAL AND IMAGINARY PARTS
C
      DO 6 J = 1, N
       K = (J-1) * MDIM +1
       L = K + N
       CALL SCOPY(N,A(K+1),2,WORK(1),1)
       CALL SCOPY(N,A(K),2,A(K),1)
       CALL SCOPY(N,WORK(1),1,A(L),1)
    6 CONTINUE
C
C     SCALE AND ORTHOGONAL REDUCTION TO HESSENBERG.
C
      CALL CBAL(MDIM,N,A(1),A(N+1),ILO,IHI,WORK(1))
      CALL CORTH(MDIM,N,ILO,IHI,A(1),A(N+1),WORK(N+1),WORK(2*N+1))
      IF(JOB .NE. 0) GO TO 10
C
C     EIGENVALUES ONLY
C
      CALL COMQR(MDIM,N,ILO,IHI,A(1),A(N+1),E(1),E(N+1),INFO)
      GO TO 30
C
C     EIGENVALUES AND EIGENVECTORS.
C
   10 CALL COMQR2(MDIM,N,ILO,IHI,WORK(N+1),WORK(2*N+1),A(1),A(N+1),
     1  E(1),E(N+1),V(1),V(N+1),INFO)
      IF (INFO .NE. 0) GO TO 30
      CALL CBABK2(MDIM,N,ILO,IHI,WORK(1),N,V(1),V(N+1))
C
C     CONVERT EIGENVECTORS TO COMPLEX STORAGE.
C
      DO 20 J = 1,N
       K = (J-1) * MDIM + 1
       I = (J-1) * 2 * LDV + 1
       L = K + N
       CALL SCOPY(N,V(K),1,WORK(1),1)
       CALL SCOPY(N,V(L),1,V(I+1),2)
       CALL SCOPY(N,WORK(1),1,V(I),2)
   20 CONTINUE
C
C     CONVERT EIGENVALUES TO COMPLEX STORAGE.
C
   30 CALL SCOPY(N,E(1),1,WORK(1),1)
      CALL SCOPY(N,E(N+1),1,E(2),2)
      CALL SCOPY(N,WORK(1),1,E(1),2)
      RETURN
C
C     TAKE CARE OF N=1 CASE
C
   35 E(1) = A(1)
      E(2) = A(2)
      INFO = 0
      IF(JOB .EQ. 0) RETURN
      V(1) = A(1)
      V(2) = A(2)
      RETURN
      END