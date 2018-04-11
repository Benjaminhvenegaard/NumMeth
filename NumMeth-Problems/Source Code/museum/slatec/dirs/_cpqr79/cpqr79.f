      SUBROUTINE CPQR79 (NDEG, COEFF, ROOT, IERR, WORK)
      COMPLEX COEFF(*), ROOT(*), SCALE, C
      REAL WORK(*)
      INTEGER NDEG, IERR, K, KHR, KHI, KWR, KWI, KAD, KJ
C***FIRST EXECUTABLE STATEMENT  CPQR79
      IERR = 0
      IF (ABS(COEFF(1)) .EQ. 0.0) THEN
         IERR = 2
         CALL XERMSG ('SLATEC', 'CPQR79',
     +      'LEADING COEFFICIENT IS ZERO.', 2, 1)
         RETURN
      ENDIF
C
      IF (NDEG .LE. 0) THEN
         IERR = 3
         CALL XERMSG ('SLATEC', 'CPQR79', 'DEGREE INVALID.', 3, 1)
         RETURN
      ENDIF
C
      IF (NDEG .EQ. 1) THEN
         ROOT(1) = -COEFF(2)/COEFF(1)
         RETURN
      ENDIF
C
      SCALE = 1.0E0/COEFF(1)
      KHR = 1
      KHI = KHR+NDEG*NDEG
      KWR = KHI+KHI-KHR
      KWI = KWR+NDEG
C
      DO 10 K=1,KWR
         WORK(K) = 0.0E0
   10 CONTINUE
C
      DO 20 K=1,NDEG
         KAD = (K-1)*NDEG+1
         C = SCALE*COEFF(K+1)
         WORK(KAD) = -REAL(C)
         KJ = KHI+KAD-1
         WORK(KJ) = -AIMAG(C)
         IF (K .NE. NDEG) WORK(KAD+K) = 1.0E0
   20 CONTINUE
C
      CALL COMQR (NDEG,NDEG,1,NDEG,WORK(KHR),WORK(KHI),WORK(KWR),
     1   WORK(KWI),IERR)
C
      IF (IERR .NE. 0) THEN
         IERR = 1
         CALL XERMSG ('SLATEC', 'CPQR79',
     +      'NO CONVERGENCE IN 30 QR ITERATIONS.', 1, 1)
         RETURN
      ENDIF
C
      DO 30 K=1,NDEG
         KM1 = K-1
         ROOT(K) = CMPLX(WORK(KWR+KM1),WORK(KWI+KM1))
   30 CONTINUE
      RETURN
      END
