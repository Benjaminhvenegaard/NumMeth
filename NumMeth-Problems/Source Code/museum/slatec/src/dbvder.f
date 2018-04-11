      SUBROUTINE DBVDER (X, Y, YP, G, IPAR)
      INTEGER IGOFX, INHOMO, IPAR, IVP, J, K, L, NA, NCOMP, NFC, NOFST
      DOUBLE PRECISION C, G(*), X, XSAV, Y(*), YP(*)
C
C **********************************************************************
C
      COMMON /DML8SZ/ C,XSAV,IGOFX,INHOMO,IVP,NCOMP,NFC
C
C **********************************************************************
C     The COMMON block below is used to communicate with the user
C     supplied subroutine DFMAT.  The user should not alter this
C     COMMON block.
C
      COMMON /DMLIVP/ NOFST
C **********************************************************************
C
C***FIRST EXECUTABLE STATEMENT  DBVDER
      IF (IVP .GT. 0) CALL DUIVP(X,Y(IVP+1),YP(IVP+1))
      NOFST = IVP
      NA = 1
      DO 10 K=1,NFC
         CALL DFMAT(X,Y(NA),YP(NA))
         NOFST = NOFST - NCOMP
         NA = NA + NCOMP
   10 CONTINUE
C
      IF (INHOMO .NE. 1) RETURN
      CALL DFMAT(X,Y(NA),YP(NA))
C
      IF (IGOFX .EQ. 0) RETURN
      IF (X .NE. XSAV) THEN
         IF (IVP .EQ. 0) CALL DGVEC(X,G)
         IF (IVP .GT. 0) CALL DUVEC(X,Y(IVP+1),G)
         XSAV = X
      ENDIF
C
C     If the user has chosen not to normalize the particular
C     solution, then C is defined in DBVPOR to be 1.0
C
C     The following loop is just
C     CALL DAXPY (NCOMP, 1.0D0/C, G, 1, YP(NA), 1)
C
      DO 20 J=1,NCOMP
         L = NA + J - 1
         YP(L) = YP(L) + G(J)/C
   20 CONTINUE
      RETURN
      END
