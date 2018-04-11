      SUBROUTINE BNDACC (G, MDG, NB, IP, IR, MT, JT)
      DIMENSION G(MDG,*)
C***FIRST EXECUTABLE STATEMENT  BNDACC
      ZERO=0.
C
C              ALG. STEPS 1-4 ARE PERFORMED EXTERNAL TO THIS SUBROUTINE.
C
      NBP1=NB+1
      IF (MT.LE.0.OR.NB.LE.0) RETURN
C
      IF(.NOT.MDG.LT.IR) GO TO 5
      NERR=1
      IOPT=2
      CALL XERMSG ('SLATEC', 'BNDACC', 'MDG.LT.IR, PROBABLE ERROR.',
     +   NERR, IOPT)
      RETURN
    5 CONTINUE
C
C                                             ALG. STEP 5
      IF (JT.EQ.IP) GO TO 70
C                                             ALG. STEPS 6-7
      IF (JT.LE.IR) GO TO 30
C                                             ALG. STEPS 8-9
      DO 10 I=1,MT
        IG1=JT+MT-I
        IG2=IR+MT-I
        DO 10 J=1,NBP1
        G(IG1,J)=G(IG2,J)
   10 CONTINUE
C                                             ALG. STEP 10
      IE=JT-IR
      DO 20 I=1,IE
        IG=IR+I-1
        DO 20 J=1,NBP1
        G(IG,J)=ZERO
   20 CONTINUE
C                                             ALG. STEP 11
      IR=JT
C                                             ALG. STEP 12
   30 MU=MIN(NB-1,IR-IP-1)
      IF (MU.EQ.0) GO TO 60
C                                             ALG. STEP 13
      DO 50 L=1,MU
C                                             ALG. STEP 14
        K=MIN(L,JT-IP)
C                                             ALG. STEP 15
        LP1=L+1
        IG=IP+L
        DO 40 I=LP1,NB
          JG=I-K
          G(IG,JG)=G(IG,I)
   40 CONTINUE
C                                             ALG. STEP 16
        DO 50 I=1,K
        JG=NBP1-I
        G(IG,JG)=ZERO
   50 CONTINUE
C                                             ALG. STEP 17
   60 IP=JT
C                                             ALG. STEPS 18-19
   70 MH=IR+MT-IP
      KH=MIN(NBP1,MH)
C                                             ALG. STEP 20
      DO 80 I=1,KH
        CALL H12 (1,I,MAX(I+1,IR-IP+1),MH,G(IP,I),1,RHO,
     1            G(IP,I+1),1,MDG,NBP1-I)
   80 CONTINUE
C                                             ALG. STEP 21
      IR=IP+KH
C                                             ALG. STEP 22
      IF (KH.LT.NBP1) GO TO 100
C                                             ALG. STEP 23
      DO 90 I=1,NB
        G(IR-1,I)=ZERO
   90 CONTINUE
C                                             ALG. STEP 24
  100 CONTINUE
C                                             ALG. STEP 25
      RETURN
      END
