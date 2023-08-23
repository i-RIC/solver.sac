      SUBROUTINE HYDIE_CGNS
     I                (CGNSID,TBUNT,FTEMP,NID,IDLST
     M                 ,FLGS,SI,FILST)
C
C     + + + PURPOSE + + +
C     THIRD DEVELOPEMENT PROGRAM.  USES A WSPRO DATA SET AS THE DATA BASE
C     AND NOT A WDM FILE AS IN PREVIOUS VERSIONS.
C
      USE iric
C     + + + DUMMY ARGUMENTS + + +
      INTEGER CGNSID,TBUNT,NID,FLGS(NID),FILST,FTEMP(2),SI
      CHARACTER*16 IDLST(NID)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SI      - units flag input/output 0-fts/fts 1-ms/ms 2-fts/ms 3-ms/fts
C     CGNSID  - iRIC CGNS/HDF5 file identifier (replaces FCARD)
C     TBUNT   - file unit no. for table output
C     FTEMP(1)   - file unit no. for scratch file
C     FTEMP(2)   - file unit no. for error printing, <0 prints to screen and
C                  file no. and output
C     NID     - number of cross sections to select
C     IDLST   - (NID) array of section id's to select from datafile
C     FLGS    - (NID) array of branch numbers for IDLST array on input.
C               On return the array contains search and error flags
C               for section id's. 0- found, -1-not found, -2- read error.
C     FILST   - file status flags 0- does entire data file, no files are
C               opened when the subroutine is called.  1-multi branch
C               file, temp file doesn't exist, no files openned.
C               2- temp file doesn't exist, input and output files open.
C               3- temp file exists, all files open, returns this value
C               on exit if successful files.
C               -1 no files opened, does only selected cross sections.
C               (Opening errors only, no read errors)

C     + + + PARAMETERS + + +
C     NCORD=no. of coordinate pairs,  NSUBS=no. of subareas,  NSBS=2(nsubs+1)
C     NXSEC=no. of cross sections

      INTEGER NXSEC,NCORD,NSUBS,NSBS
      PARAMETER (NXSEC=200, NCORD=150, NSUBS=19, NSBS=40)

C     + + + LOCAL VARIABLES + + +
      INTEGER NKTS,I,NCOMP,K,IREC,II,N,ERR,EDF,NRUF,NOUT,MATCH,
     #       NCARDS,XSREC(NXSEC),KXND,TBTYP(3)
     #       ,FNDXSC,J,ISEC,OPOUT(17),NSA,BRNF,NSECT
     #       ,IHDR,IOVER,ERRIO,ERRNO,IOLD,NSTOP,KTS,KK,SL1

      REAL S(NCORD),G(NCORD),XSA(NSUBS),POUT(17,1),SIN(3,NSBS),
     #  RATIO,RUF(3,NSBS),SECPRP(3,NXSEC),MAXDEP,X(NXSEC),SRD,SRD_FT
     #     COMP(6,5),MINDEP,SUBPRP(7,NSUBS),DATUM
      DOUBLE PRECISION GX(NXSEC),GY(NXSEC),DX,DY
      CHARACTER*16 SECID,ELID(NXSEC)
      CHARACTER*75 TITLES(3)

C     + + + EXTERNALS + + +
      INTEGER MATSTR,IXMAX,IXMIN
      EXTERNAL WSPDA,WSPSEQ,KNOTS,WSPCDS,PROPER,STAGE,HYDOUT,
     #         FNDXSC,OUTTAB,IXMIN,BRNOUT,MATSTR,IXMAX
C
C     + + + INSTRINSICS + + +
      INTRINSIC INT
C
C     + + + INITIALIZATIONS + + +
      DATA IOVER /1000/
C
      ERRIO=FTEMP(2)
      KTS=1
      NCARDS=1
      IREC=1
      NCOMP=0
      TBTYP(1)=0

      OPEN(UNIT=FTEMP(1),STATUS='SCRATCH',ACCESS='DIRECT',
     #     FORM='FORMATTED',RECL=80)
      IJUNK=0
      OPEN(UNIT=TBUNT,STATUS='SCRATCH')

240   FORMAT (A75)
      DO I=1,3
        TITLES(I) = '                                                  '
        WRITE(TBUNT,240)TITLES(I)
      END DO

      NSA=-1
      N=0
      NRUF=0
      SRD = 0

      CALL CG_IRIC_READ_GRID2D_COORDS(CGNSID,GX,GY,IER)

      CALL CG_IRIC_READ_COMPLEX_COUNT(
     #  CGNSID,'Crosssection',ISEC,IER)

      DO 35 K=ISEC,1,-1
        EDF=0
        SL1=16

        DO I=1,NSBS
          SIN(1,I)=0.0
          SIN(2,I)=1.0
          SIN(3,I)=0.0
        END DO

        IF(K /= ISEC) THEN
          DX = GX(K + 1) - GX(K)
          DY = GY(K + 1) - GY(K)
          SRD = SRD + SQRT(DX*DX + DY*DY)
        ENDIF

        IF(SI.EQ.1.OR.SI.EQ.3)THEN
          SRD_FT = SRD *(100./30.48)
        ELSE
          SRD_FT = SRD
        ENDIF

        CALL WSPCDS_CGNS(CGNSID,FTEMP,K,S,G,N,NSA,XSA,NRUF,RUF
     #              ,SECPRP,SECID,SRD,SI,EDF)
        IF(EDF.GT.0) GO TO 35

        NKTS=1
        CALL KNOTS(NKTS,SECPRP(1,K),SECPRP(3,K),RATIO,X,ERR)

        IF(EDF.GT.1) FLGS(K)=-1*EDF
        IF(EDF.GT.0) GO TO 35
        DO II=1,NKTS
          CALL PROPER(S,G,N,NSA,XSA,X(II),NRUF,RUF,SIN,POUT,SUBPRP,
     #                KXND)
C         * write property tables *
          CALL SLPOUT(TBUNT,SRD_FT,SECID,KTS,POUT,NSA,SUBPRP)
        END DO
 35   CONTINUE
      FILST=3
C
      RETURN
      END

C-----Purpose:
C       convert int value to simple string
C     Programmed by: Keisuke Inoue

      CHARACTER*10 FUNCTION ITOSTR(intval)
        INTEGER, INTENT(IN):: intval
        
        CHARACTER*10 buf
 300    FORMAT(I10)
        WRITE(buf,300), intval
        ITOSTR = TRIM(ADJUSTL(buf))
      END FUNCTION ITOSTR
      
C-----Purpose:
C       convert float value to simple string
C     Programmed by: Keisuke Inoue

      CHARACTER*10 FUNCTION FTOSTR(floatval)
        REAL, INTENT(IN):: floatval
        INTEGER:: strlen
        
        CHARACTER*10 buf
 301    FORMAT(F10.4)
        WRITE(buf,301), floatval
        buf = ADJUSTL(buf)
        
 302    CONTINUE
        strlen = len_trim(buf)
        IF (buf(strlen:strlen) == '0') THEN
          buf = buf(1:strlen-1)
          GOTO 302
        ENDIF
        strlen = len_trim(buf)
        IF (buf(strlen:strlen) == '.') THEN
          buf = buf(1:strlen-1)
        ENDIF
        FTOSTR = TRIM(buf)
      END FUNCTION FTOSTR

      SUBROUTINE WSPCDS_CGNS
     I                 (CGNSID,FTUNIT,K,S,G,N,NSA,XSA,NRUF,
     O                  RUF,SECPRP,SECID,SRD,SI,EDFG)
C
C     + + +  PURPOSE + + +
C     Reads a single x-section from a WSPRO DA file into the arrays
C     several WSPRO card types are ignored, including
C     bridge, spur dike, road section and culvert header cards and
C     data.  Skew angle correction fixed on 1.17.97
C
C     + + + PARAMETERS + + +
      USE iric
      INTEGER NCORD,NSUBS,NSBS,NXSEC
      PARAMETER (NXSEC=200,NCORD=150, NSUBS=19, NSBS=40)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER CGNSID
      INTEGER FTUNIT(2),K,NRUF,N,NSA,EDFG,SI
      REAL S(NCORD),G(NCORD),XSA(NSUBS),RUF(3,NSBS),
     #     SECPRP(3,NXSEC),SRD,N_SIMPLE
      CHARACTER*16 SECID
      CHARACTER*16 SECIDNUM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CGNSID     - iRIC CGNS/HDF5 file identifier
C     FTUNIT(1)  - unit on which temp DA file is openned
C     FTUNIT(2)  - unit on which error messages are printed
C     K       - cross section ID (starts from 1)
C     S       - station point
C     G       - ground elevation
C     N       - no. of station points and groung elevation pairs
C     NSA     - no. of subsection areas
C     XSA     - array of subsection righthand breakpoints.
C     NRUF    - no. of roughness values
C     RUF     - array of roughness data
C     
C     SECID   - section I.D.
C     SRD     - cross section reference distance in input unit
C     EDFG    - error flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,J,NCHAR,IO,IXMIN,ERRIO,ERRNO,RREC,IOVER,N_TYPE,IER
      REAL SKEW,SKEW_ANGLE
      REAL N_TMP(NCORD),BOTD(NCORD),TOPD(NCORD),BOTN(NCORD),TOPN(NCORD)
      REAL POS(NCORD),WSE(NCORD)
      INTEGER WSE_OK
      CHARACTER  CBUF*77

      CHARACTER*10 ITOSTR,FTOSTR
      EXTERNAL ITOSTR,FTOSTR

C     + + + INTRINSIC + + +
      INTRINSIC COS,FLOAT

C     + + + DATA INITIALIZATIONS + + +
      DATA IOVER / 1000 /

C     + + + INPUT FORMAT + + +
 100  FORMAT(A5,A5,A77)
 101  FORMAT(A)

      ERRIO=0
      NCHAR=70
      SKEW=1.0
      RREC=IREC
      
      SKEW=1
      
      CALL CG_IRIC_READ_COMPLEX_STRING(
     #  CGNSID, 'Crosssection', K, 'name', SECID, IER)
     
      WRITE(ERRIO,101) 'XS   ' // SECID(1:5) // TRIM(FTOSTR(SRD))
     
      CALL CG_IRIC_READ_COMPLEX_INTEGER(
     #  CGNSID, 'Crosssection', K, 'n_type', N_TYPE, IER)

      IF (N_TYPE == 0) THEN
C       Simple
        CALL CG_IRIC_READ_COMPLEX_FUNCTIONALSIZE(
     #    CGNSID, 'Crosssection', K, 'appr_xs', N, IER)
        CALL CG_IRIC_READ_COMPLEX_FUNCTIONAL_REALSINGLE_1D(
     #    CGNSID, 'Crosssection', K, 'appr_xs', S, G, IER)
        CALL CG_IRIC_READ_COMPLEX_REALSINGLE(
     #    CGNSID, 'Crosssection', K, 'n_simple', N_SIMPLE, IER)

        NRUF=1
        RUF(1,1)=1
        RUF(2,1)=N_SIMPLE
        RUF(3,1)=0
        NSA=1
        XSA(1)=0

      ELSE IF (N_TYPE == 1) THEN
C       Horizontal distribution
        CALL CG_IRIC_READ_COMPLEX_FUNCTIONALSIZE(
     #    CGNSID, 'Crosssection', K, 'n_h', N, IER)
        CALL CG_IRIC_READ_COMPLEX_FUNCTIONALWITHNAME_REALSINGLE(
     #    CGNSID, 'Crosssection', K, 'n_h','X', S, IER)
        CALL CG_IRIC_READ_COMPLEX_FUNCTIONALWITHNAME_REALSINGLE(
     #    CGNSID, 'Crosssection', K, 'n_h','Y', G, IER)
        CALL CG_IRIC_READ_COMPLEX_FUNCTIONALWITHNAME_REALSINGLE(
     #    CGNSID, 'Crosssection', K, 'n_h', 'NVAL', N_TMP, IER)

        NRUF=1
        RUF(1,1)=1
        RUF(2,1)=N_TMP(2)
        RUF(3,1)=0
        NSA=1
        XSA(1)=0

        DO I=3,N
          IF (N_TMP(I).NE.RUF(2,NRUF)) THEN
            NRUF=NRUF+1
            RUF(1,NRUF)=NRUF
            RUF(2,NRUF)=N_TMP(I)
            RUF(3,NRUF)=0
            NSA=NSA+1
            XSA(NSA-1)=(S(I-1))
          ENDIF
        END DO

      ELSE IF (N_TYPE == 2) THEN
C       Vertical distribution
        CALL CG_IRIC_READ_COMPLEX_FUNCTIONALSIZE(
     #    CGNSID, 'Crosssection', K, 'n_h_v', N, IER)
        CALL CG_IRIC_READ_COMPLEX_FUNCTIONALWITHNAME_REALSINGLE(
     #    CGNSID, 'Crosssection', K, 'n_h_v', 'X', S, IER)
        CALL CG_IRIC_READ_COMPLEX_FUNCTIONALWITHNAME_REALSINGLE(
     #    CGNSID, 'Crosssection', K, 'n_h_v', 'Y', G, IER)
        CALL CG_IRIC_READ_COMPLEX_FUNCTIONALWITHNAME_REALSINGLE(
     #    CGNSID, 'Crosssection', K, 'n_h_v', 'BOTD', BOTD, IER)
        CALL CG_IRIC_READ_COMPLEX_FUNCTIONALWITHNAME_REALSINGLE(
     #    CGNSID, 'Crosssection', K, 'n_h_v', 'TOPD', TOPD, IER)
        CALL CG_IRIC_READ_COMPLEX_FUNCTIONALWITHNAME_REALSINGLE(
     #    CGNSID, 'Crosssection', K, 'n_h_v','BOTN', BOTN, IER)
        CALL CG_IRIC_READ_COMPLEX_FUNCTIONALWITHNAME_REALSINGLE(
     "    CGNSID, 'Crosssection', K, 'n_h_v','TOPN', TOPN, IER)
        NRUF=2
        RUF(1,1)=1
        RUF(2,1)=BOTN(2)
        RUF(3,1)=BOTD(2)
        RUF(1,2)=1
        RUF(2,2)=TOPN(2)
        RUF(3,2)=TOPD(2)
        NSA=1
        XSA(1)=0
        
        DO I=3,N
          IF ((BOTN(I).NE.RUF(2,NRUF-1)).OR.
     #        (BOTD(I).NE.RUF(3,NRUF-1)).OR.
     #        (TOPN(I).NE.RUF(2,NRUF)).OR.
     #        (TOPD(I).NE.RUF(3,NRUF))) THEN
            NRUF=NRUF+2
            RUF(1,NRUF-1)=NRUF/2
            RUF(2,NRUF-1)=BOTN(I)
            RUF(3,NRUF-1)=BOTD(I)
            RUF(1,NRUF  )=NRUF/2
            RUF(2,NRUF  )=TOPN(I)
            RUF(3,NRUF  )=TOPD(I)
            NSA=NSA+1
            XSA(NSA-1)=(S(I-1))
          ENDIF
        END DO
      ENDIF

      WSE_OK = 1
      CALL CG_IRIC_READ_FUNCTIONAL_REALSINGLE_1D(
     #  CGNSID, 'wse', POS, WSE, IER)
      IF (IER /= 0) THEN
        WSE_OK = 0
      ENDIF
      
      SECPRP(1,K) = WSE(K)
     
      SECPRP(2,K)=0
      SECPRP(3,K)=0

C     Output log

      CBUF = ''
      DO I=0, N-1
        IF (MOD(I, 3) == 0 .AND. I /= 0) THEN
          WRITE(ERRIO,101) 'GR        ' // TRIM(CBUF)
          CBUF = ''
        ENDIF
        
        CBUF = TRIM(ADJUSTL(CBUF)) // ' ' // TRIM(FTOSTR(S(I+1))) //
     #    ',' // TRIM(FTOSTR(G(I+1)))
      END DO
      IF (TRIM(CBUF) /= '') THEN
        WRITE(ERRIO,101) 'GR        ' // TRIM(ADJUSTL(CBUF))
      ENDIF

      IF (N_TYPE == 0) THEN
        WRITE(ERRIO,101) 'N         ' // TRIM(FTOSTR(N_SIMPLE))
      ELSE IF (N_TYPE == 1) THEN
        CBUF = ''
        DO I=0, NRUF-1
          IF (MOD(I, 6) == 0 .AND. I /= 0) THEN
            WRITE(ERRIO,101) 'N         ' // TRIM(CBUF)
            CBUF = ''
          ENDIF
          CBUF = TRIM(ADJUSTL(CBUF)) // ' ' // TRIM(FTOSTR(RUF(2, I+1)))
        END DO
        IF (TRIM(CBUF) /= '') THEN
          WRITE(ERRIO,101) 'N         ' // TRIM(ADJUSTL(CBUF))
        ENDIF
        
        CBUF = ''
        DO I=0, NSA-2
          IF (MOD(I, 6) == 0 .AND. I /= 0) THEN
            WRITE(ERRIO,101) 'SA        ' // TRIM(CBUF)
          ENDIF
          CBUF = TRIM(ADJUSTL(CBUF)) // ' ' // TRIM(FTOSTR(XSA(I+1)))
        END DO
        IF (TRIM(CBUF) /= '') THEN
          WRITE(ERRIO,101) 'SA        ' // TRIM(CBUF)
        ENDIF
      ELSE IF (N_TYPE == 2) THEN
        CBUF = ''
        DO I=0, (NRUF/2)-1
          IF (MOD(I, 6) == 0 .AND. I /= 0) THEN
            WRITE(ERRIO,101) 'N         ' // TRIM(CBUF)
            CBUF = ''
          ENDIF
          CBUF = TRIM(ADJUSTL(CBUF)) // ' ' //
     #      TRIM(FTOSTR(RUF(2, I*2+1))) // ',' //
     #      TRIM(FTOSTR(RUF(2, I*2+2)))
        END DO
        IF (TRIM(CBUF) /= '') THEN
          WRITE(ERRIO,101) 'N         ' // TRIM(ADJUSTL(CBUF))
        ENDIF
             
        CBUF = ''
        DO I=0, (NRUF/2)-1
          IF (MOD(I, 6) == 0 .AND. I /= 0) THEN
            WRITE(ERRIO,101) 'ND        ' // TRIM(CBUF)
            CBUF = ''
          ENDIF
          CBUF = TRIM(ADJUSTL(CBUF)) // ' ' //
     #      TRIM(FTOSTR(RUF(3, I*2+1))) // ',' // 
     #      TRIM(FTOSTR(RUF(3, I*2+2)))
        END DO
        IF (TRIM(CBUF) /= '') THEN
          WRITE(ERRIO,101) 'ND        ' // TRIM(ADJUSTL(CBUF))
        ENDIF

        CBUF = ''
        DO I=0, NSA-2
          IF (MOD(I, 6) == 0 .AND. I /= 0) THEN
            WRITE(ERRIO,101) 'SA        ' // TRIM(CBUF)
          ENDIF
          CBUF = TRIM(ADJUSTL(CBUF)) // ' ' // TRIM(FTOSTR(XSA(I+1)))
        END DO
        IF (TRIM(CBUF) /= '') THEN
          WRITE(ERRIO,101) 'SA        ' // TRIM(ADJUSTL(CBUF))
        ENDIF
      ENDIF

      WRITE(ERRIO,101) 'HP   ' // SECID(1:5) // TRIM(FTOSTR(WSE(K)))

      DO I=1,N
        S(I)=S(I) * SKEW
      END DO
      DO I=1,NSA
        XSA(I)=XSA(I) * SKEW
      END DO

      IF(N.LE.0) THEN
        ERRNO=17
        CALL ERRPRT(ERRIO,ERRNO,IOVER,SECID)
        EDFG=2
      ELSE IF(NRUF.LE.0) THEN
        ERRNO=18
        CALL ERRPRT(ERRIO,ERRNO,IOVER,SECID)
        EDFG=2
      ENDIF
      IF (WSE_OK == 0) THEN
        ERRNO=22
        CALL ERRPRT(ERRIO,ERRNO,IOVER,SECID)
      ENDIF

C
      IF(SI.EQ.1.OR.SI.EQ.3)THEN
C       convert meters to feet
        CALL MTRFT(N,S)
        CALL MTRFT(N,G)
        CALL MTRFT(NSA,XSA)
        SECPRP(1,K) = SECPRP(1,K)*(100./30.48)
      ENDIF
C
      RETURN
      END
