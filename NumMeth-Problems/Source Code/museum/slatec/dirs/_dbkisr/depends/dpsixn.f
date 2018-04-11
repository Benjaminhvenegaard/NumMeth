      DOUBLE PRECISION FUNCTION DPSIXN (N)
C
      INTEGER N, K
      DOUBLE PRECISION AX, B, C, FN, RFN2, TRM, S, WDTOL
      DOUBLE PRECISION D1MACH
      DIMENSION B(6), C(100)
C
C             DPSIXN(N), N = 1,100
      DATA C(1), C(2), C(3), C(4), C(5), C(6), C(7), C(8), C(9), C(10),
     1     C(11), C(12), C(13), C(14), C(15), C(16), C(17), C(18),
     2     C(19), C(20), C(21), C(22), C(23), C(24)/
     3    -5.77215664901532861D-01,     4.22784335098467139D-01,
     4     9.22784335098467139D-01,     1.25611766843180047D+00,
     5     1.50611766843180047D+00,     1.70611766843180047D+00,
     6     1.87278433509846714D+00,     2.01564147795561000D+00,
     7     2.14064147795561000D+00,     2.25175258906672111D+00,
     8     2.35175258906672111D+00,     2.44266167997581202D+00,
     9     2.52599501330914535D+00,     2.60291809023222227D+00,
     1     2.67434666166079370D+00,     2.74101332832746037D+00,
     2     2.80351332832746037D+00,     2.86233685773922507D+00,
     3     2.91789241329478063D+00,     2.97052399224214905D+00,
     4     3.02052399224214905D+00,     3.06814303986119667D+00,
     5     3.11359758531574212D+00,     3.15707584618530734D+00/
      DATA C(25), C(26), C(27), C(28), C(29), C(30), C(31), C(32),
     1     C(33), C(34), C(35), C(36), C(37), C(38), C(39), C(40),
     2     C(41), C(42), C(43), C(44), C(45), C(46), C(47), C(48)/
     3     3.19874251285197401D+00,     3.23874251285197401D+00,
     4     3.27720405131351247D+00,     3.31424108835054951D+00,
     5     3.34995537406483522D+00,     3.38443813268552488D+00,
     6     3.41777146601885821D+00,     3.45002953053498724D+00,
     7     3.48127953053498724D+00,     3.51158256083801755D+00,
     8     3.54099432554389990D+00,     3.56956575411532847D+00,
     9     3.59734353189310625D+00,     3.62437055892013327D+00,
     1     3.65068634839381748D+00,     3.67632737403484313D+00,
     2     3.70132737403484313D+00,     3.72571761793728215D+00,
     3     3.74952714174680596D+00,     3.77278295570029433D+00,
     4     3.79551022842756706D+00,     3.81773245064978928D+00,
     5     3.83947158108457189D+00,     3.86074817682925274D+00/
      DATA C(49), C(50), C(51), C(52), C(53), C(54), C(55), C(56),
     1     C(57), C(58), C(59), C(60), C(61), C(62), C(63), C(64),
     2     C(65), C(66), C(67), C(68), C(69), C(70), C(71), C(72)/
     3     3.88158151016258607D+00,     3.90198967342789220D+00,
     4     3.92198967342789220D+00,     3.94159751656514710D+00,
     5     3.96082828579591633D+00,     3.97969621032421822D+00,
     6     3.99821472884273674D+00,     4.01639654702455492D+00,
     7     4.03425368988169777D+00,     4.05179754953082058D+00,
     8     4.06903892884116541D+00,     4.08598808138353829D+00,
     9     4.10265474805020496D+00,     4.11904819067315578D+00,
     1     4.13517722293122029D+00,     4.15105023880423617D+00,
     2     4.16667523880423617D+00,     4.18205985418885155D+00,
     3     4.19721136934036670D+00,     4.21213674247469506D+00,
     4     4.22684262482763624D+00,     4.24133537845082464D+00,
     5     4.25562109273653893D+00,     4.26970559977879245D+00/
      DATA C(73), C(74), C(75), C(76), C(77), C(78), C(79), C(80),
     1     C(81), C(82), C(83), C(84), C(85), C(86), C(87), C(88),
     2     C(89), C(90), C(91), C(92), C(93), C(94), C(95), C(96)/
     3     4.28359448866768134D+00,     4.29729311880466764D+00,
     4     4.31080663231818115D+00,     4.32413996565151449D+00,
     5     4.33729786038835659D+00,     4.35028487337536958D+00,
     6     4.36310538619588240D+00,     4.37576361404398366D+00,
     7     4.38826361404398366D+00,     4.40060929305632934D+00,
     8     4.41280441500754886D+00,     4.42485260777863319D+00,
     9     4.43675736968339510D+00,     4.44852207556574804D+00,
     1     4.46014998254249223D+00,     4.47164423541605544D+00,
     2     4.48300787177969181D+00,     4.49424382683587158D+00,
     3     4.50535493794698269D+00,     4.51634394893599368D+00,
     4     4.52721351415338499D+00,     4.53796620232542800D+00,
     5     4.54860450019776842D+00,     4.55913081598724211D+00/
      DATA C(97), C(98), C(99), C(100)/
     1     4.56954748265390877D+00,     4.57985676100442424D+00,
     2     4.59006084263707730D+00,     4.60016185273808740D+00/
C             COEFFICIENTS OF ASYMPTOTIC EXPANSION
      DATA B(1), B(2), B(3), B(4), B(5), B(6)/
     1     8.33333333333333333D-02,    -8.33333333333333333D-03,
     2     3.96825396825396825D-03,    -4.16666666666666666D-03,
     3     7.57575757575757576D-03,    -2.10927960927960928D-02/
C
C***FIRST EXECUTABLE STATEMENT  DPSIXN
      IF (N.GT.100) GO TO 10
      DPSIXN = C(N)
      RETURN
   10 CONTINUE
      WDTOL = MAX(D1MACH(4),1.0D-18)
      FN = N
      AX = 1.0D0
      S = -0.5D0/FN
      IF (ABS(S).LE.WDTOL) GO TO 30
      RFN2 = 1.0D0/(FN*FN)
      DO 20 K=1,6
        AX = AX*RFN2
        TRM = -B(K)*AX
        IF (ABS(TRM).LT.WDTOL) GO TO 30
        S = S + TRM
   20 CONTINUE
   30 CONTINUE
      DPSIXN = S + LOG(FN)
      RETURN
      END