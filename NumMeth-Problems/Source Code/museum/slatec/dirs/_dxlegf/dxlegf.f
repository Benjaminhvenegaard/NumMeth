      SUBROUTINE DXLEGF (DNU1, NUDIFF, MU1, MU2, THETA, ID, PQA, IPQA,
     1   IERROR)
      DOUBLE PRECISION PQA,DNU1,DNU2,SX,THETA,X,PI2
      DIMENSION PQA(*),IPQA(*)
C
C***FIRST EXECUTABLE STATEMENT  DXLEGF
      IERROR=0
      CALL DXSET (0, 0, 0.0D0, 0,IERROR)
      IF (IERROR.NE.0) RETURN
      PI2=2.D0*ATAN(1.D0)
C
C        ZERO OUTPUT ARRAYS
C
      L=(MU2-MU1)+NUDIFF+1
      DO 290 I=1,L
      PQA(I)=0.D0
  290 IPQA(I)=0
C
C        CHECK FOR VALID INPUT VALUES
C
      IF(NUDIFF.LT.0) GO TO 400
      IF(DNU1.LT.-.5D0) GO TO 400
      IF(MU2.LT.MU1) GO TO 400
      IF(MU1.LT.0) GO TO 400
      IF(THETA.LE.0.D0.OR.THETA.GT.PI2) GO TO 420
      IF(ID.LT.1.OR.ID.GT.4) GO TO 400
      IF((MU1.NE.MU2).AND.(NUDIFF.GT.0)) GO TO 400
C
C        IF DNU1 IS NOT AN INTEGER, NORMALIZED P(MU,DNU,X)
C        CANNOT BE CALCULATED.  IF DNU1 IS AN INTEGER AND
C        MU1.GT.DNU2 THEN ALL VALUES OF P(+MU,DNU,X) AND
C        NORMALIZED P(MU,NU,X) WILL BE ZERO.
C
      DNU2=DNU1+NUDIFF
      IF((ID.EQ.3).AND.(MOD(DNU1,1.D0).NE.0.D0)) GO TO 295
      IF((ID.EQ.4).AND.(MOD(DNU1,1.D0).NE.0.D0)) GO TO 400
      IF((ID.EQ.3.OR.ID.EQ.4).AND.MU1.GT.DNU2) RETURN
  295 CONTINUE
C
      X=COS(THETA)
      SX=1.D0/SIN(THETA)
      IF(ID.EQ.2) GO TO 300
      IF(MU2-MU1.LE.0) GO TO 360
C
C        FIXED NU, VARIABLE MU
C        CALL DXPMU TO CALCULATE P(-MU1,NU,X),....,P(-MU2,NU,X)
C
      CALL DXPMU(DNU1,DNU2,MU1,MU2,THETA,X,SX,ID,PQA,IPQA,IERROR)
      IF (IERROR.NE.0) RETURN
      GO TO 380
C
  300 IF(MU2.EQ.MU1) GO TO 320
C
C        FIXED NU, VARIABLE MU
C        CALL DXQMU TO CALCULATE Q(MU1,NU,X),....,Q(MU2,NU,X)
C
      CALL DXQMU(DNU1,DNU2,MU1,MU2,THETA,X,SX,ID,PQA,IPQA,IERROR)
      IF (IERROR.NE.0) RETURN
      GO TO 390
C
C        FIXED MU, VARIABLE NU
C        CALL DXQNU TO CALCULATE Q(MU,DNU1,X),....,Q(MU,DNU2,X)
C
  320 CALL DXQNU(DNU1,DNU2,MU1,THETA,X,SX,ID,PQA,IPQA,IERROR)
      IF (IERROR.NE.0) RETURN
      GO TO 390
C
C        FIXED MU, VARIABLE NU
C        CALL DXPQNU TO CALCULATE P(-MU,DNU1,X),....,P(-MU,DNU2,X)
C
  360 CALL DXPQNU(DNU1,DNU2,MU1,THETA,ID,PQA,IPQA,IERROR)
      IF (IERROR.NE.0) RETURN
C
C        IF ID = 3, TRANSFORM P(-MU,NU,X) VECTOR INTO
C        P(MU,NU,X) VECTOR.
C
  380 IF(ID.EQ.3) CALL DXPMUP(DNU1,DNU2,MU1,MU2,PQA,IPQA,IERROR)
      IF (IERROR.NE.0) RETURN
C
C        IF ID = 4, TRANSFORM P(-MU,NU,X) VECTOR INTO
C        NORMALIZED P(MU,NU,X) VECTOR.
C
      IF(ID.EQ.4) CALL DXPNRM(DNU1,DNU2,MU1,MU2,PQA,IPQA,IERROR)
      IF (IERROR.NE.0) RETURN
C
C        PLACE RESULTS IN REDUCED FORM IF POSSIBLE
C        AND RETURN TO MAIN PROGRAM.
C
  390 DO 395 I=1,L
      CALL DXRED(PQA(I),IPQA(I),IERROR)
      IF (IERROR.NE.0) RETURN
  395 CONTINUE
      RETURN
C
C        *****     ERROR TERMINATION     *****
C
  400 CALL XERMSG ('SLATEC', 'DXLEGF',
     +             'DNU1, NUDIFF, MU1, MU2, or ID not valid', 210, 1)
      IERROR=210
      RETURN
  420 CALL XERMSG ('SLATEC', 'DXLEGF', 'THETA out of range', 211, 1)
      IERROR=211
      RETURN
      END