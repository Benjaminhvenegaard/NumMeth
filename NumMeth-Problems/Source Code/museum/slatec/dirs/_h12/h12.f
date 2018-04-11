      SUBROUTINE H12 (MODE, LPIVOT, L1, M, U, IUE, UP, C, ICE, ICV, NCV)
      DIMENSION U(IUE,*), C(*)
C***FIRST EXECUTABLE STATEMENT  H12
      ONE=1.
C
      IF (0.GE.LPIVOT.OR.LPIVOT.GE.L1.OR.L1.GT.M) RETURN
      CL=ABS(U(1,LPIVOT))
      IF (MODE.EQ.2) GO TO 60
C                            ****** CONSTRUCT THE TRANSFORMATION. ******
          DO 10 J=L1,M
   10     CL=MAX(ABS(U(1,J)),CL)
      IF (CL) 130,130,20
   20 CLINV=ONE/CL
      SM=(U(1,LPIVOT)*CLINV)**2
          DO 30 J=L1,M
   30     SM=SM+(U(1,J)*CLINV)**2
      CL=CL*SQRT(SM)
      IF (U(1,LPIVOT)) 50,50,40
   40 CL=-CL
   50 UP=U(1,LPIVOT)-CL
      U(1,LPIVOT)=CL
      GO TO 70
C            ****** APPLY THE TRANSFORMATION  I+U*(U**T)/B  TO C. ******
C
   60 IF (CL) 130,130,70
   70 IF (NCV.LE.0) RETURN
      B=UP*U(1,LPIVOT)
C                       B  MUST BE NONPOSITIVE HERE.  IF B = 0., RETURN.
C
      IF (B) 80,130,130
   80 B=ONE/B
      MML1P2=M-L1+2
      IF (MML1P2.GT.20) GO TO 140
      I2=1-ICV+ICE*(LPIVOT-1)
      INCR=ICE*(L1-LPIVOT)
          DO 120 J=1,NCV
          I2=I2+ICV
          I3=I2+INCR
          I4=I3
          SM=C(I2)*UP
              DO 90 I=L1,M
              SM=SM+C(I3)*U(1,I)
   90         I3=I3+ICE
          IF (SM) 100,120,100
  100     SM=SM*B
          C(I2)=C(I2)+SM*UP
              DO 110 I=L1,M
              C(I4)=C(I4)+SM*U(1,I)
  110         I4=I4+ICE
  120     CONTINUE
  130 RETURN
  140 CONTINUE
      L1M1=L1-1
      KL1=1+(L1M1-1)*ICE
      KL2=KL1
      KLP=1+(LPIVOT-1)*ICE
      UL1M1=U(1,L1M1)
      U(1,L1M1)=UP
      IF (LPIVOT.EQ.L1M1) GO TO 150
      CALL SSWAP(NCV,C(KL1),ICV,C(KLP),ICV)
  150 CONTINUE
          DO 160 J=1,NCV
          SM=SDOT(MML1P2,U(1,L1M1),IUE,C(KL1),ICE)
          SM=SM*B
          CALL SAXPY (MML1P2,SM,U(1,L1M1),IUE,C(KL1),ICE)
          KL1=KL1+ICV
  160 CONTINUE
      U(1,L1M1)=UL1M1
      IF (LPIVOT.EQ.L1M1) RETURN
      KL1=KL2
      CALL SSWAP(NCV,C(KL1),ICV,C(KLP),ICV)
      RETURN
      END
