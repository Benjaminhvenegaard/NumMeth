      SUBROUTINE DFC (NDATA, XDATA, YDATA, SDDATA, NORD, NBKPT, BKPT,
     +   NCONST, XCONST, YCONST, NDERIV, MODE, COEFF, W, IW)
      DOUBLE PRECISION BKPT(*), COEFF(*), SDDATA(*), W(*), XCONST(*),
     *   XDATA(*), YCONST(*), YDATA(*)
      INTEGER IW(*), MODE, NBKPT, NCONST, NDATA, NDERIV(*), NORD
C
      EXTERNAL DFCMN
C
      INTEGER I1, I2, I3, I4, I5, I6, I7, MDG, MDW
C
C***FIRST EXECUTABLE STATEMENT  DFC
      MDG = NBKPT - NORD + 3
      MDW = NBKPT - NORD + 1 + NCONST
C                        USAGE IN DFCMN( ) OF W(*)..
C     I1,...,I2-1      G(*,*)
C
C     I2,...,I3-1      XTEMP(*)
C
C     I3,...,I4-1      PTEMP(*)
C
C     I4,...,I5-1      BKPT(*) (LOCAL TO DFCMN( ))
C
C     I5,...,I6-1      BF(*,*)
C
C     I6,...,I7-1      W(*,*)
C
C     I7,...           WORK(*) FOR DLSEI( )
C
      I1 = 1
      I2 = I1 + MDG*(NORD+1)
      I3 = I2 + MAX(NDATA,NBKPT)
      I4 = I3 + MAX(NDATA,NBKPT)
      I5 = I4 + NBKPT
      I6 = I5 + NORD*NORD
      I7 = I6 + MDW*(NBKPT-NORD+1)
      CALL DFCMN(NDATA, XDATA, YDATA, SDDATA, NORD, NBKPT, BKPT, NCONST,
     1   XCONST, YCONST, NDERIV, MODE, COEFF, W(I5), W(I2), W(I3),
     2   W(I4), W(I1), MDG, W(I6), MDW, W(I7), IW)
      RETURN
      END
