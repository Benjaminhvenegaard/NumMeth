      SUBROUTINE SSPEV (A, N, E, V, LDV, WORK, JOB, INFO)
      INTEGER I,INFO,J,LDV,M,N
      REAL A(*),E(*),V(LDV,*),WORK(*)
C***FIRST EXECUTABLE STATEMENT  SSPEV
       IF (N .GT. LDV) CALL XERMSG ('SLATEC', 'SSPEV', 'N .GT. LDV.',
     +    1, 1)
       IF(N .GT. LDV) RETURN
       IF (N .LT. 1) CALL XERMSG ('SLATEC', 'SSPEV', 'N .LT. 1', 2, 1)
       IF(N .LT. 1) RETURN
C
C       CHECK N=1 CASE
C
      E(1) = A(1)
      INFO = 0
      IF(N .EQ. 1) RETURN
C
      IF(JOB.NE.0) GO TO 20
C
C     EIGENVALUES ONLY
C
      CALL TRED3(N,1,A,E,WORK(1),WORK(N+1))
      CALL TQLRAT(N,E,WORK(N+1),INFO)
      RETURN
C
C     EIGENVALUES AND EIGENVECTORS
C
   20 CALL TRED3(N,1,A,E,WORK(1),WORK(1))
      DO 30 I = 1, N
        DO 25 J = 1, N
   25     V(I,J) = 0.
   30   V(I,I) = 1.
      CALL IMTQL2(LDV,N,E,WORK,V,INFO)
      M = N
      IF(INFO .NE. 0) M = INFO - 1
      CALL TRBAK3(LDV,N,1,A,M,V)
      RETURN
      END
