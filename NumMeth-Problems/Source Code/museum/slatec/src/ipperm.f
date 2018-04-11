      SUBROUTINE IPPERM (IX, N, IPERM, IER)
      INTEGER IX(*), N, IPERM(*), I, IER, INDX, INDX0, ITEMP, ISTRT
C***FIRST EXECUTABLE STATEMENT  IPPERM
      IER=0
      IF(N.LT.1)THEN
         IER=1
         CALL XERMSG ('SLATEC', 'IPPERM',
     +    'The number of values to be rearranged, N, is not positive.',
     +    IER, 1)
         RETURN
      ENDIF
C
C     CHECK WHETHER IPERM IS A VALID PERMUTATION
C
      DO 100 I=1,N
         INDX=ABS(IPERM(I))
         IF((INDX.GE.1).AND.(INDX.LE.N))THEN
            IF(IPERM(INDX).GT.0)THEN
               IPERM(INDX)=-IPERM(INDX)
               GOTO 100
            ENDIF
         ENDIF
         IER=2
         CALL XERMSG ('SLATEC', 'IPPERM',
     +    'The permutation vector, IPERM, is not valid.', IER, 1)
         RETURN
  100 CONTINUE
C
C     REARRANGE THE VALUES OF IX
C
C     USE THE IPERM VECTOR AS A FLAG.
C     IF IPERM(I) > 0, THEN THE I-TH VALUE IS IN CORRECT LOCATION
C
      DO 330 ISTRT = 1 , N
         IF (IPERM(ISTRT) .GT. 0) GOTO 330
         INDX = ISTRT
         INDX0 = INDX
         ITEMP = IX(ISTRT)
  320    CONTINUE
         IF (IPERM(INDX) .GE. 0) GOTO 325
            IX(INDX) = IX(-IPERM(INDX))
            INDX0 = INDX
            IPERM(INDX) = -IPERM(INDX)
            INDX = IPERM(INDX)
            GOTO 320
  325    CONTINUE
         IX(INDX0) = ITEMP
  330 CONTINUE
C
      RETURN
      END
