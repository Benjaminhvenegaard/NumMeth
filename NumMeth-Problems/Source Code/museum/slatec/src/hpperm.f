      SUBROUTINE HPPERM (HX, N, IPERM, WORK, IER)
      INTEGER N, IPERM(*), I, IER, INDX, INDX0, ISTRT
      CHARACTER*(*) HX(*), WORK
C***FIRST EXECUTABLE STATEMENT  HPPERM
      IER=0
      IF(N.LT.1)THEN
         IER=1
         CALL XERMSG ('SLATEC', 'HPPERM',
     +    'The number of values to be rearranged, N, is not positive.',
     +    IER, 1)
         RETURN
      ENDIF
      IF(LEN(WORK).LT.LEN(HX(1)))THEN
         IER=2
         CALL XERMSG ('SLATEC', 'HPPERM',
     +    'The length of the work variable, WORK, is too short.',IER,1)
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
         IER=3
         CALL XERMSG ('SLATEC', 'HPPERM',
     +    'The permutation vector, IPERM, is not valid.', IER, 1)
         RETURN
  100 CONTINUE
C
C     REARRANGE THE VALUES OF HX
C
C     USE THE IPERM VECTOR AS A FLAG.
C     IF IPERM(I) > 0, THEN THE I-TH VALUE IS IN CORRECT LOCATION
C
      DO 330 ISTRT = 1 , N
         IF (IPERM(ISTRT) .GT. 0) GOTO 330
         INDX = ISTRT
         INDX0 = INDX
         WORK = HX(ISTRT)
  320    CONTINUE
         IF (IPERM(INDX) .GE. 0) GOTO 325
            HX(INDX) = HX(-IPERM(INDX))
            INDX0 = INDX
            IPERM(INDX) = -IPERM(INDX)
            INDX = IPERM(INDX)
            GOTO 320
  325    CONTINUE
         HX(INDX0) = WORK
  330 CONTINUE
C
      RETURN
      END
