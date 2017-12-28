      PROGRAM SAC
C
C     SAC is a reogranized version of the C374 slope-area program
C     that uses HYDIE as the cross section property computations.  This
C     allows use of WSPRO data records as input into the program.
C     modified 11-3-94:JMF-bug fix
C
      INTEGER NLINES
      CHARACTER*5 VERS
      PARAMETER (NLINES=60)
C     + + + LOCAL VARIABLES + + +
      INTEGER HUNIT(3),ERROR,NXSEC,IOPT,MINC,I,J,P,NSA(30),PNO
     #      ,PVAIL,LEFT,IWARN,IER,SI
      REAL  SRD(30),DELH(30),A(30),QNVEY(30),ALPH(30),
     #      TOPW(30),WPERM(30),HYDRD(30),STAGE(30),Q,SUMH,
     #      PHI,CX,RC,RX,SPREAD,L,TOTK,TOTA,TOTW,VELO,FROUDE,K1,K2,
     #      SLOPE,ALZHA,VELHED,NROUGH(30),SUBPRP(30,7,20),FSUB,
     #      VSUB,K1SUB,K2SUB,RSUB,QRATIO,DREACH
      CHARACTER*16 SECID(30)
      CHARACTER*80 RTITLE
      CHARACTER*75 TITLES(3)
      CHARACTER*4 CER
      CHARACTER*1 SLPCODE
      CHARACTER*132 ARG1
      DATA VERS/'2.0'/
C     version number changed on 9/6/2012 from 2011a to 2.0 per K.Flynn's suggestion.  No other changes to source code.
C     version 2011a code revised the previous version for batch or interactive operation
C     and only allows use of WSPRO type files.  Changes mainly in HYDSLP routine for reading files.
C     does not allow browsing for files in windows.
C
C     + + + FORMATS + + +
  144 FORMAT (/29X,'DISCHARGE COMPUTATIONS'/,22X,'Reach')
  145 FORMAT (/29X,'DISCHARGE COMPUTATIONS'/,11X,'K weighted  Reach')
  146 FORMAT (13X,'dH,fall  length  Discharge  Spread'
     #        ,'    HF     CX     RC     RX  ER',/,15X,'(ft)     (ft)'
     #        ,5X,'(cfs)     (%)',4X,'(ft)')
  147 FORMAT(1X,A4,' - ',A4,1X,F6.2,2X,F7.0,2X,F9.0,3X,I4,2X,F6.3,
     #       1X,3(F6.3,1X),A4)
  151 FORMAT(2X,'I.D. ',A16,7X,'Velocity head ',F7.2,'ft    Discharge ',
     #       F9.0,'cfs',/,'   Ref.distance ',F9.0,'ft',12X,
     #       ' Q/K   ',F7.4,7X,' Alpha ',F5.3,'  ',//,'  Sub  Water'
     #       ,18X,'Top   ','Wetted   Hydraulic   Conveyance',/,
     #       ' area surface   n',5X,'Area   width perimeter   radius',
     #       '   x 0.001',6X,'Vel.',4X,'F',/,'  no. el.(ft)',7X,
     #       '(sq.ft)   (ft)   (ft)',7X,'(ft)',5X,'(cfs)   %   (fps)' )
  153 FORMAT(I5,1X,F7.2,1X,F5.3,1X,F8.1,1X,F7.1,1X,F6.1,4X,F6.2,3X,
     #       F8.3,1X,F4.0,1X,F4.1,2X,F4.2)
  155 FORMAT('Total',1X,F7.2,'   --- ',F8.0,1X,F7.0,1X,F6.0,4X,F6.2,3X,
     #       F8.3,1X,F4.0,1X,F4.1,2X,F4.2)
  156 FORMAT('----------------------------------------',
     #       '----------------------------------------')
  246 FORMAT (13X,'dH,fall  length  Discharge  Spread'
     #        ,'    HF     CX     RC     RX  ER',/,15X,'(m)      (m) '
     #        ,5X,'(cms)     (%)',4X,'(m)')
  247 FORMAT(1X,A4,' - ',A4,F7.3,1X,F8.1,1X,F10.1,3X,I4,1X,F7.4,
     #       1X,3(F6.3,1X),A4)
  251 FORMAT(2X,'I.D. ',A16,7X,'Velocity head ',F7.2,'m     Discharge ',
     #       F9.0,'cms',/,'   Ref.distance ',F9.0,'m ',12X,
     #       ' Q/K   ',F7.4,7X,' Alpha ',F5.3,'  ',//,'  Sub  Water'
     #       ,18X,'Top   ','Wetted   Hydraulic   Conveyance',/,
     #       ' area surface   n',5X,'Area   width perimeter   radius',
     #       '   x 0.001',6X,'Vel.',4X,'F',/,'  no. el.(m) ',7X,
     #       '(sq. m)   (m)    (m) ',7X,'(m) ',5X,'(cms)   %   (mps)' )
  253 FORMAT(I5,F8.3,1X,F5.3,F9.2,1X,F7.2,1X,F6.2,4X,F6.2,3X,
     #       F8.3,1X,F4.0,1X,F3.2,2X,F4.2)
  255 FORMAT('Total',1X,F7.2,'   --- ',F8.0,1X,F7.0,1X,F6.0,4X,F6.2,3X,
     #       F8.3,1X,F4.0,1X,F4.1,2X,F4.2)
700   FORMAT(A1,'SAC -USGS slope-area program Ver ',A5,34X,'page',I3,/)
701   FORMAT('Definitions:',/,
     #2X,'Spread, the percent difference between ',
     #'discharge computed with no expansion',/,6X,'loss (k=0) ',
     #'and discharge computed with full expansion loss (k=1.0),',
     #' divided',/,6X,'by the discharge computed with full',
     #' expansion loss',/,2X,
     #'HF, friction head- HF = sum of Q*Q*L/(K1*K2) over subreaches;',
     #' Q, discharge;',/,6X,'L, reach length; K1, upstream section',
     #' conveyance;',/,6X,'K2, downstream section conveyance'
     #,/,2X,'CX, the computed discharge divided by the discharge com',
     #'puted with no expansion',/,6X,'loss (k=0)',/
     #2X,'RC, velocity head change in contracting section divided by ',
     #'friction head',/,2X,'RX, velocity head change in expanding ',
     #'section divided by friction head',/,2X,'ER, warnings, *-fall <'
     #' 0.5ft, @-conveyance ratio exceeded, #-reach too short',/,
     #6X,'error, 1-negative or 0 fall',/,
     #2X,'******, terms that can not be computed because'
     #' of strong expansion in reach')
702   FORMAT(A80)
703   FORMAT('Definitions:',/,
     #2X,'n, Manning''s coefficient of roughness    Q/K = discharge/c',
     #'onveyance',/,
     #2X,'F, Froude number ',
     #'F = Ki*Q/(K*A sqrt(g*(Ai/TWi)); Q, discharge; A, total cross-',
     #/,6X,
     #'section area; g, acceleration of gravity; Ai, sub-section area;',
     #' TWi, sub-',/,6X,'section top width')
 704  FORMAT(A75)
 705  FORMAT(/30X,'CROSS  SECTION  PROPERTIES')
C
C ----  READ CROSS SECTION DATA AND COMPUTE PROPERTIES --
C
      PNO=1
      P=10
      IOPT=1
      SI=0
  15  CONTINUE
C     call statements here to hydie or c374in
      HUNIT(1)=8
      HUNIT(2)=9
      HUNIT(3)=11
      NSEC=30

      CALL GETARG(1,ARG1)
      IF (ARG1 == 'Case1.cgn') THEN
        CALL HYDSLP_CGNS(HUNIT,NSEC,VERS,ERROR,RTITLE,SLPCODE,SI)
        IF(ERROR.NE.0) GO TO 9999
        CALL RDTABL(HUNIT(3),NSEC,SLPCODE,NXSEC,SECID,SRD,DELH,A,
     #              QNVEY,ALPH,TOPW,WPERM,HYDRD,STAGE,
     #              NROUGH,NSA,SUBPRP,TITLES)
        IF(NXSEC.LE.0) GO TO 9999
        P=6
      ELSE
        CALL HYDSLP(HUNIT,NSEC,VERS,ERROR,RTITLE,SLPCODE,SI)
        IF(ERROR.NE.0) GO TO 9999
        CALL RDTABL(HUNIT(3),NSEC,SLPCODE,NXSEC,SECID,SRD,DELH,A,
     #              QNVEY,ALPH,TOPW,WPERM,HYDRD,STAGE,
     #              NROUGH,NSA,SUBPRP,TITLES)
        IF(NXSEC.LE.0) GO TO 9999
      ENDIF
C
C ----  BEGIN FLOW-COMPUTATION PHASE --------------------
C
  830 CONTINUE
      WRITE (P,700) CHAR(12),VERS,PNO
      WRITE (P,702) RTITLE
      WRITE (P,704) (TITLES(I),I=1,3)
      IF(SLPCODE.EQ.'1')THEN
        WRITE(P,145)
      ELSE
        WRITE(P,144)
      ENDIF
      IF(SI.EQ.0.OR.SI.EQ.3)THEN
        WRITE (P,146)
      ELSE
        WRITE (P,246)
      ENDIF
      MINC = 1
      PVAIL=NLINES-24
      IF(IOPT.EQ.2) MINC = NXSEC-2
      DO  91  M = 2,NXSEC,MINC
        LEFT=PVAIL-(NXSEC-M+2)
        IF(LEFT.LT.0)THEN
C          DO 20 ILINES=1,PVAIL
C            WRITE(P,*)
C  20      CONTINUE
          WRITE(P,701)
          PNO=PNO+1
          WRITE(P,700)CHAR(12),VERS,PNO
          WRITE(P,702)RTITLE
          WRITE (P,704) (TITLES(I),I=1,3)
          IF(SI.EQ.0.OR.SI.EQ.3)THEN
            WRITE(P,146)
          ELSE
            WRITE(P,246)
          ENDIF
          PVAIL=NLINES-24-(NXSEC-M+2)
        ELSE
          PVAIL=LEFT
        ENDIF
        DO  90  II=NXSEC,M,-1
          I=II-M+1
          CALL QSLPA (M, DELH(I), A(I), QNVEY(I),
     #     SRD(I), ALPH(I), Q, SUMH, PHI,CX, RC,RX, SPREAD,IER)
          L = ABS(SRD(I+M-1) - SRD(I))
          ISPREA = INT( 100.*AMIN1(1E3,ABS(SPREAD)))
          CER(1:4)='    '
          IF(Q.GT.0.0) THEN
            PHI=PHI*Q*Q
          ELSE
            PHI=-1E3
          ENDIF
          QRATIO=QNVEY(I+M-1)/QNVEY(I)
          DREACH=0.5*((A(I)/TOPW(I))+(A(I+M-1)/TOPW(I+M-1)))
          IF(SUMH.LT.0.5) CER(1:1)='*'
          IF(QRATIO.GT.1.25.OR.QRATIO.LT.0.80) CER(2:2)='@'
          IF(75.*DREACH.GT.L) CER(3:3)="#"
          IF(IER.EQ.1)CER(1:1)='1'
          IF(SI.EQ.0.OR.SI.EQ.3)THEN
            WRITE (P,147) SECID(I+M-1),SECID(I),SUMH,L,Q,ISPREA,PHI,
     #                  CX,RC,RX,CER
          ELSE
            WRITE (P,247) SECID(I+M-1),SECID(I),FTMTR(SUMH),FTMTR(L),
     #                    FT3M3(Q),ISPREA,FTMTR(PHI),CX,RC,RX,CER
          ENDIF
   90   CONTINUE
        WRITE(P,147)
   91 CONTINUE
      WRITE(P,701)
C
C  PRINT SUBAREA PROPERTIES AND FLOW DISTRIBUTION
C
      PVAIL=PVAIL-3
      NHDR=9
      LEFT=PVAIL-NHDR-NSA(I)-5
      IF(LEFT.GE.0) WRITE(P,705)
      DO 99 I=1,NXSEC
        TOTA = A(I)
        TOTW = TOPW(I)
        TOTK = QNVEY(I)
        IWARN=0
        ISECT=I
        LEFT=PVAIL-NHDR-NSA(I)
        IF((LEFT-5).LT.0)THEN
          IF(I.GT.1) WRITE(P,703)
C          DO 22 ILINES=1,PVAIL
C            WRITE(P,*)
C  22      CONTINUE
          PNO=PNO+1
          WRITE(P,700)CHAR(12),VERS,PNO
          WRITE(P,702)RTITLE
          WRITE (P,704) (TITLES(I2),I2=1,3)
          WRITE(P,705)
          PVAIL=NLINES-14-NSA(I)-NHDR
        ELSE
          PVAIL=LEFT
        ENDIF
        WRITE(P,156)
        ALZHA = ALPH(I)
        IF(Q.GT.0.0) THEN
          SLOPE = (Q/TOTK)**2
          VELO = Q/TOTA
          VELHED = ALZHA*VELO**2/64.4
          FROUDE = VELO/SQRT(32.2*TOTA/TOTW)
        ELSE
          SLOPE =-1E36
          FROUDE= -1E36
          VELHED= -1E36
          VELO= -1E36
        ENDIF
        IF(SI.EQ.0.OR.SI.EQ.3)THEN
          WRITE(P,151) SECID(I), VELHED, Q, SRD(I), SLOPE, ALZHA
        ELSE
          WRITE(P,251) SECID(I), FTMTR(VELHED), FT3M3(Q), FTMTR(SRD(I)),
     #                 SLOPE, ALZHA
        ENDIF
        IF(NSA(I).NE.0) THEN
c	    write(*,*)subprp(i,2,1),SUBPRP(i,1,1)
          DO 95 J=1,NSA(I)
C           subprp(i,2,j)=area; subprp(i,1,j)=topwidth;
C           subprp(i,6,j)=conveyance
            K1 = SUBPRP(I,6,J)
            K2 = K1/TOTK
c	      write(*,*)nsa(I),i,j,SUBPRP(i,2,j),SUBPRP(i,1,j)
            IF(Q.GT.0.0.AND. SUBPRP(i,2,j).GT.0.0) THEN
              VSUB = K2*Q/(SUBPRP(I,2,J))
              FSUB = VSUB/SQRT(32.2*(SUBPRP(I,2,J))/SUBPRP(I,1,J))
            ELSE
              VSUB = -1E36
              FSUB = -1E36
            ENDIF
            RSUB = SUBPRP(I,2,J)/SUBPRP(I,3,J)
            K1SUB = K1/1000.
            K2SUB = K2*100.
            IF(SI.EQ.0.OR.SI.EQ.3)THEN
              WRITE (P,153) J,SUBPRP(I,5,J),SUBPRP(I,7,J),
     #        SUBPRP(I,2,J),SUBPRP(I,1,J),SUBPRP(I,3,J),RSUB,
     #        K1SUB,K2SUB,VSUB,FSUB
            ELSE
              WRITE (P,253) J,FTMTR(SUBPRP(I,5,J)),SUBPRP(I,7,J),
     #  FT2M2(SUBPRP(I,2,J)),FTMTR(SUBPRP(I,1,J)),FTMTR(SUBPRP(I,3,J))
     #        ,FTMTR(RSUB),FT3M3(K1SUB),FT3M3(K2SUB),FTMTR(VSUB),FSUB
            ENDIF
   95     CONTINUE
        ENDIF
        K1 = TOTK/1000.
        K2 = 100.
        IF(SI.EQ.0.OR.SI.EQ.3)THEN
          WRITE (P,155) STAGE(I),A(I),TOPW(I),WPERM(I),HYDRD(I),
     #       K1,K2,VELO,FROUDE
        ELSE
          WRITE (P,255) FTMTR(STAGE(I)),FT2M2(A(I)),FTMTR(TOPW(I)),
     #       FTMTR(WPERM(I)),FTMTR(HYDRD(I)),
     #       FT3M3(K1),FT3M3(K2),FTMTR(VELO),FROUDE
        ENDIF
        WRITE(P,156)
   99 CONTINUE
      WRITE(P,703)
C
 9999 CONTINUE
      CLOSE (UNIT=P)
      STOP
C
      END
C
      SUBROUTINE QSLPA ( M, DELH,A,QNVEY,SRD,ALPH,
     $      Q,H,FPHI,CX,RC,RX,SPREAD,IER)
C
C    EVALUATES  SLOPE-AREA SOLUTION FOR DISCHARGE
C     M-SECTION FORMULA.   TWRI 3-A2.
C
C  ARGUMENTS --
C        M  --  NUMBER OF CROSS SECTIONS.
C        DELH -- W.S. FALL BETWEEN SECTIONS. VECTOR DIM M.
C          NOTE - DELH(2) IS FALL FROM 1-ST TO 2-ND X-SECS.
C              VALUE OF DELH(1) IS NOT USED.
C        A    -- VECTOR OF X-SECTION AREAS, DIM M.
C        QNVEY  --  CONVEYANCE VECTOR DIM M
C        SRD  --  SECTION REF DIST VECTOR DIM M
C        ALPH -- VECTOR OF VELOCITY-HEAD COEFFS, DIM M.
C        Q    -- RESULTANT DISCHARGE
C        H    -- TOTAL W.S.FALL, SUM OVER SUBREACHES.
C        FPHI -- FRICTION.HEAD
C        CX   -- EXPANSION-CONTRACTION COEFF - RATIO Q0/Q,
C                  WHERE Q0 = DISCHARGE UNDER ASSUMPTION OF
C                  NO EXPANSION-CONTRACTION LOSSES (I.E., 100 PCT
C                  ENERGY 'RECOVERY')
C        RC,RX -- CONTRACTION AND EXPANSION VELOCITY-HEAD-CHANGE RATIOS,
C                  RELATIVE TO FRICTION HEAD.  SEE REMARKS BELOW.
C        SPREAD -- (Q0-Q1)/Q1 -- DIFFERENCE BETWEEN DISCHARGES COMPUTED
C                    ASSUMING NO EXPANSION LOSS AND 100.PCT EXPANSION
C                    LOSS, RELATIVE TO 100% EXPANSION.
C
C  REMARKS --
C    RC,RX = SUM OF VELOCITY-HEAD CHANGES DUE TO CONTRACTION AND
C     EXPANSION, ESPECTIVELY, AS RATIO TO FRICTION HEAD.  THIS IS
C    EXPLICIT MEASURE OF DEGREE AND POTENTIAL IMPORTANCE OF CONTRACTION
C    AND EXPANSION.   (NOTE THAT SPREAD AND CX REFLECT ONLY EXPANSION
C    EFFECTS AND NOT CONTRACTIONS.)   NOTE --  RX .LE. 0.
C
C    FOR STRONGLY EXPANDING REACES, CX AND SPREAD CANNOT BE COMPUTED
C    BECAUSE Q0 IS SQUARE ROOT OF NEGATIVE NUMBER.  FRICTION LOSS IS
C    INSUFFICIENT (IN ABSENCE OF EXPANSION LOSS) TO ACCOUNT FOR W.S.FALL
C    AND VELOCITY HEADS.  BERNOULLI'S EQUATION CANNOT BE SATISFIED WITH-
C    OUT XPANSION-LOSS TERMS.  IN THIS CASE, VALUES OF -1E36 ARE RETURNED
C    FOR SPREAD AND CX.   FOR EXTREME EXPANSIONS, Q ITSELF CANNOT BE
C    COMPUTED AND -1E36 IS RETURNED.   IN THESE CASES, PHI AND RX WILL
C    BE LARGE NEGATIVE NUMBERS.
C
C    DISCHARGE FOR ANY COMBINATION OF EXPANSION AND CONTRACTION
C    LOSS COEFFS CAN BE COMPUTED AS FOLLOWS --
C          QKKP =  Q * SQRT( 1./( 1./CX**2 + PHI*(KP*RC - K*RX) ) )
C    WHERE (K,KP) ARE (EXPANSION,CONTRACTION) LOSS COEFFS.
C    (IN USGS, KP USUALLY = 0.,  K = 0.5.)
C
C    RELATIVE CHANGE IN Q DUE TO CHANGE IN K,KP CAN BE ESTIMATED
C    APPROXIMATELY AS FOLLOWS --
C          (QKKP-Q)/Q = -.5 * ( KP*RC - (K-.5)*RX )
C
C    IER - error code returned: 
C          1-flat/no slope in water surface in reach or negative fall
C          in reach.
C          2- expansion losses and friction losses
C          summed are less than the negative change in velocity head.
C          Large expansion is possible.
C          3- expansion losses and friction losses exactly balance the
C          negative change in velocity head
C
      REAL  DELH(M),A(M),QNVEY(M),SRD(M),ALPH(M)
      REAL K, KP
      DATA  K, KP / 0.5, 0.0 /
      DATA  G / 32.2 /
C
      IER = 0
      Q      = -1E36
      SPREAD = -1E36
      CX   =  -1E36
      FPHI  =  -1E36
      H = 0.
      FSUM = 0.
      ESUM = 0.
      DO 55 I=1,M-1
        H = H + DELH(I)
        FSUM = FSUM + ABS(SRD(I)-SRD(I+1))/(QNVEY(I)*QNVEY(I+1))
        ESUM = ESUM + ABS( ALPH(I)/A(I)**2 - ALPH(I+1)/A(I+1)**2 )
55    CONTINUE
C      FCESUM=0.
C      FXESUM=0.
C      DO 56 I=1,M-1
C      RI= ALPH(I)/A(I)**2 - ALPH(I+1)/A(I+1)**2
C      WRITE(*,*)'RI',RI
C      IF(RI.LT.0)THEN
C        FXESUM = FXESUM +ABS(RI)
C      ELSE IF(RI.GT.0)THEN
C        FCESUM = FCESUM + ABS(RI)
C      ENDIF
C56    CONTINUE
C      CSUM=FCESUM/(2.*G)
C      XSUM=(-1.)*FXESUM/(2.*G)
      ESUM = ESUM
      DSUM = ( ALPH(1)/A(1)**2 - ALPH(M)/A(M)**2 )
      CSUM = (DSUM + ESUM)/(4.*G)
      XSUM = (DSUM - ESUM)/(4.*G)
      RD = DSUM/(2.*G*FSUM)
      RC = CSUM/FSUM
      RX = XSUM/FSUM
      PHII  = 1. + RD + KP*RC - K*RX
      IF(ABS(PHII).GT.0.) FPHI = FSUM
      PHII0 = 1. + RD
      PHII1 = 1. + RD  -  1.0*RX
C
      IF(H .LE. 0.) THEN
C       negative or zero fall in reach
        IER = 1
      ELSE IF(PHII0 .LE. 0.) THEN
C       upstream velocity head much larger than downstream
C       velocity head making velocity term much larger than friction term.
C       indicative of rapid changes in reach geometry (expansion)
        IF(ABS(PHII).LE.0.)  THEN
C         expansion losses and friction losses exactly balance the negative
C         change in velocity head
          IER = 3
        ELSE
C         expansion losses and friction losses summed are less than the
C         negative change in velocity head- large expansion possible
          QSQ = H/(FSUM*PHII)
          Q  =  SQRT( QSQ )
          IER = 2
        ENDIF
      ELSE
        Q0 = SQRT( H/(FSUM*PHII0) )
        Q1 = SQRT( H/(FSUM*PHII1) )
        Q  = SQRT( H/(FSUM*PHII ) )
        FPHI = FSUM
        IF(Q.GT.0.) THEN
          SPREAD = (Q0-Q1)/Q1
          CX =  Q/Q0
        ENDIF
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE HYDSLP
     #                 (HUNIT,NSEC,VERS,ERROR,RTITLE,SLPCODE,SI)
C
C     + + + PURPOSE + + +
C     HYDSLP is the data program for the modified c374 slope-area
C     program that uses WSPRO or HYDRO input data with HYDIE for the
C     hydraulic properties computation.
C
C     + + + PARAMETERS + + +
      INTEGER MSEC
      PARAMETER (MSEC=30)
C     + + + ARGUMENTS + + +
      INTEGER HUNIT(3),NSEC,ERROR,SI
      CHARACTER*1 SLPCODE
      CHARACTER*5 VERS
      CHARACTER*80 RTITLE
C
C     + + + LOCAL VARIABLES + + +
      INTEGER FCARD,TBUNT,FTEMP(2),NID,INTYP,FLGS(30),FILST,I,OFILE
      CHARACTER*1 OPT
      CHARACTER*3 CODE
      CHARACTER*16 ELID(MSEC)
      CHARACTER*132 TBNAM,CDNAM,BLOT
C
C     + + + EXTERNALS + + +
      EXTERNAL HYDIE,BLOT
C
C     + + + FORMATS + + +
702   FORMAT(A80)
700   FORMAT('SAC -USGS slope-area program Ver ',A5,34X,'page  0',/)
C
      ERROR=0

100   CONTINUE
C     set filst for shydie to print out slope pgm tables
      FILST=-2
      NID=NSEC
      IF(NID.GT.MSEC) NID=MSEC
      FCARD=HUNIT(1)
      FTEMP(1)=HUNIT(2)
      FTEMP(2)=-10
      TBUNT=HUNIT(3)
      CDNAM=BLOT(CDNAM)
      TBNAM=BLOT(TBNAM)
      DO 15, I=1,NID
        FLGS(I)=1
 15   CONTINUE
C

C      modified to allow files and title in by piping command line, CDNAM, TBNAM, RTITLE where CDNAM is WSPRO file name, 
C      TBNAM is the program output file and RTITLE is the text title line in the output file.  RTITLE can have up to 80 
C      characters.  CDNAM and TBNAM can have file path names of up to 132 characters.
 10   CONTINUE
       INTYP=1
       CALL GETARG(1,CDNAM)
       CALL GETARG(2,TBNAM)
       CALL GETARG(3,RTITLE)
       INTYP=2
C
      IF (LEN_TRIM(CDNAM).eq.0) THEN
C     * open WSPRO file contianing input data and transfer into temp DA *
         WRITE(*,*)' '
         WRITE(*,*)'**** SAC - slope-area program ****'
         WRITE(*,*)'        version ',VERS
         WRITE(*,*)' '
         WRITE(*,*)'ENTER INPUT FILE (WSPRO) NAME  '
         READ(*,'(A)') CDNAM
         WRITE(*,*)'ENTER OUTPUT FILE NAME  '
         READ(*,'(A)')TBNAM
         WRITE(*,*)'ENTER HEADER TITLE FOR OUTPUT (<= 80 chars) '
         WRITE(*,900)
 900     FORMAT ('12345678901234567890123456789012345678901234567890'
     #        ,'123456789012345678901234567890')
         READ(*,'(A)') RTITLE
      ENDIF
C
C
      IF(INTYP.EQ.4.OR.ERROR.NE.0) GO TO 99

C     code modified to allow title to be entered as 3rd parameter on execute command line

      OFILE=ABS(FTEMP(2))
      OPEN(OFILE,FILE=TBNAM,STATUS='NEW')
      WRITE(OFILE,700) VERS
      WRITE(OFILE,'(A)')'Echo input data file '
      WRITE(OFILE,702) RTITLE
c      write(*,*)"pre hydie", TBUNT, FCARD, CDNAM
      CALL HYDIE(FCARD,TBUNT,FTEMP,CDNAM,TBNAM,INTYP,NID,ELID
     #          ,FLGS,SI,FILST)
C
C     get what type of elevation weighting to use, bank average (slpcode=' ')
C     or conveyance weighted( slpcode='1' ) slpcode='9' is error
      REWIND(UNIT=FCARD)
   5  CONTINUE
        READ(FCARD,'(A3,3X,A1)',END=998) CODE,SLPCODE
        IF(CODE.NE.'*TT') GO TO 5
c        IF(OPT.NE.'4') SLPCODE='9'
 998  CONTINUE
      IF(CODE.NE.'*TT') SLPCODE=' '
      CLOSE(UNIT=FCARD)
      CLOSE(UNIT=FTEMP(1))
      REWIND(UNIT=TBUNT)
C
99    CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE RDTABL
     I                 (TBUNT,NSEC,SLPCODE,
     O                  NXSEC,SECID,SRD,DELH,A,QNVEY,ALPH,TOPW,WPERM,
     O                  HYDRD,STAGE,NROUGH,NSA,SUBPRP,TITLES)
C
C     + + + PURPOSE + + +
C     reads table of hydraulic properties computed by the HYDIE program
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TBUNT  - unit no. of table computed by HYDIE
C     NSEC   - number of x-sections allowed in data set
C     NXSEC  - number of x-sections read from TBUNT file
C     SRD    - x-section reference distance in feet?
C     DELH   - fall in feet? between cross sections, computed from given stages
C     A      - total cross section area
C     QNVEY  - total conveyance
C     ALPHA  - velocity coefficient
C     TOPW   - top width
C     WPERM  - wetted perimeter
C     HYDRD  - hydraulic radius
C     STAGE  - stage
C     NROUGH - manning's composite roughness
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER TBUNT,NSEC,NXSEC,NSA(NSEC)
      REAL SRD(NSEC),DELH(NSEC),A(NSEC),QNVEY(NSEC),ALPH(NSEC),
     #     TOPW(NSEC),WPERM(NSEC),HYDRD(NSEC),STAGE(NSEC),
     #     NROUGH(NSEC),SUBPRP(NSEC,7,20),SUMKH
      CHARACTER*1 SLPCODE
      CHARACTER*16 SECID(NSEC)
      CHARACTER*75 TITLES(3),STUFF
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I
      REAL STAGE0,LEL,REL
C
C     + + + FORMATS + + +
200   FORMAT (A16,1X,F10.1,1X,F10.2,3(1X,E13.6),
     #        2(1X,F7.2))
210   FORMAT (2(1X,E13.6),1X,F5.3)
220   FORMAT (I5)
230   FORMAT (6(E13.6,1X),F6.4)
240   FORMAT (A75)
C
      DO 5 I=1,3
        READ(TBUNT,240)TITLES(I)
 5    CONTINUE
      I=0
      STAGE0=0
10    CONTINUE
      I=I+1
      READ(TBUNT,200,END=99) SECID(I),SRD(I),STAGE(I),QNVEY(I),A(I),
     #     ALPH(I),LEL,REL
      READ(TBUNT,210)TOPW(I),WPERM(I),NROUGH(I)
      READ(TBUNT,220) NSA(I)
      READ(TBUNT,230)((SUBPRP(I,K,INSA),K=1,7),INSA=1,NSA(I))
      IF(WPERM(I).GT.0)THEN
        HYDRD(I)=A(I)/WPERM(I)
      ELSE
        HYDRD(I)=0.0
      ENDIF
      IF(SLPCODE.NE.'1')THEN
        IF(REL.GT.STAGE(I).OR.LEL.GT.STAGE(I))THEN
C       error- stage(i) should always be >= rel or lel unless error in
C       data entry
          WRITE(*,*)'  '
          WRITE(*,*)'FATAL ERROR------ stage entered on HP record is '
          WRITE(*,*)'less than the water-surface elevation at a bank'
          WRITE(*,*)'*Please check GR & HP records for cross section* ',
     #            SECID(NSEC)
          NXSEC=0
          GO TO 999
        ENDIF
        IF(ABS(REL-LEL).GE.0.0) THEN
          STAGE(I)=(REL+LEL)/2.0000
        ENDIF
      ELSE
C       conveyance weighting of w.s. elevations to compute falls
        SUMKH=0.0
        DO 20 K=1,NSA(I)
          SUMKH = SUMKH+SUBPRP(I,5,K)*SUBPRP(I,6,K)
  20    CONTINUE
        STAGE(I)=SUMKH/QNVEY(I)
      ENDIF
      GO TO 10
99    CONTINUE
      NXSEC=I-1
      STAGE0=STAGE(NXSEC)
      DO 25 I=NXSEC,1,-1
        DELH(I) = STAGE0-STAGE(I)
        STAGE0=STAGE(I)
25    CONTINUE
      CLOSE(UNIT=TBUNT)
C
999   CONTINUE
      RETURN
      END
      
