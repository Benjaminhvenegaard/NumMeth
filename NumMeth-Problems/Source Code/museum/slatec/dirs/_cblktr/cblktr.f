      SUBROUTINE CBLKTR (IFLG, NP, N, AN, BN, CN, MP, M, AM, BM, CM,
     +   IDIMY, Y, IERROR, W)
C
      DIMENSION       AN(*)      ,BN(*)      ,CN(*)      ,AM(*)      ,
     1                BM(*)      ,CM(*)      ,Y(IDIMY,*) ,W(*)
      EXTERNAL        PROC       ,PROCP      ,CPROC      ,CPROCP
      COMMON /CCBLK/  NPP        ,K          ,EPS        ,CNV        ,
     1                NM         ,NCMPLX     ,IK
      COMPLEX         AM         ,BM         ,CM         ,Y
C***FIRST EXECUTABLE STATEMENT  CBLKTR
      NM = N
      M2 = M+M
      IERROR = 0
      IF (M-5) 101,102,102
  101 IERROR = 1
      GO TO 119
  102 IF (NM-3) 103,104,104
  103 IERROR = 2
      GO TO 119
  104 IF (IDIMY-M) 105,106,106
  105 IERROR = 3
      GO TO 119
  106 NH = N
      NPP = NP
      IF (NPP) 107,108,107
  107 NH = NH+1
  108 IK = 2
      K = 1
  109 IK = IK+IK
      K = K+1
      IF (NH-IK) 110,110,109
  110 NL = IK
      IK = IK+IK
      NL = NL-1
      IWAH = (K-2)*IK+K+6
      IF (NPP) 111,112,111
C
C     DIVIDE W INTO WORKING SUB ARRAYS
C
  111 IW1 = IWAH
      IWBH = IW1+NM
      W(1) = IW1-1+MAX(2*NM,12*M)
      GO TO 113
  112 IWBH = IWAH+NM+NM
      IW1 = IWBH
      W(1) = IW1-1+MAX(2*NM,12*M)
      NM = NM-1
C
C SUBROUTINE CCMPB COMPUTES THE ROOTS OF THE B POLYNOMIALS
C
  113 IF (IERROR) 119,114,119
  114 IW2 = IW1+M2
      IW3 = IW2+M2
      IWD = IW3+M2
      IWW = IWD+M2
      IWU = IWW+M2
      IF (IFLG) 116,115,116
  115 CALL CCMPB (NL,IERROR,AN,BN,CN,W(2),W(IWAH),W(IWBH))
      GO TO 119
  116 IF (MP) 117,118,117
C
C SUBROUTINE CBLKT1 SOLVES THE LINEAR SYSTEM
C
  117 CALL CBLKT1 (NL,AN,BN,CN,M,AM,BM,CM,IDIMY,Y,W(2),W(IW1),W(IW2),
     1             W(IW3),W(IWD),W(IWW),W(IWU),PROC,CPROC)
      GO TO 119
  118 CALL CBLKT1 (NL,AN,BN,CN,M,AM,BM,CM,IDIMY,Y,W(2),W(IW1),W(IW2),
     1             W(IW3),W(IWD),W(IWW),W(IWU),PROCP,CPROCP)
  119 CONTINUE
      RETURN
      END
