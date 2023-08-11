      SUBROUTINE HYDSLP_CGNS
     #           (HUNIT,NSEC,VERS,ERR,RTITLE,SLPCODE,SI)
      IMPLICIT NONE
      
      include 'cgnslib_f.h'

C     + + + PURPOSE + + +
C     HYDSLP_CGNS is the subroutine to read data from CGNS file.
C     program that uses WSPRO or HYDRO input data with HYDIE for the
C     hydraulic properties computation.
C
C     + + + PARAMETERS + + +
      INTEGER MSEC
      PARAMETER (MSEC=30)
C     + + + ARGUMENTS + + +
      INTEGER HUNIT(3),NSEC,ERR,SI
      CHARACTER*1 SLPCODE
      CHARACTER*5 VERS
      CHARACTER*80 RTITLE
C
C     + + + LOCAL VARIABLES + + +
      INTEGER TBUNT,FTEMP(2),NID,FLGS(30),FILST,I,OFILE
      INTEGER CGNSID,IER
      INTEGER SLPCODE_INT
      CHARACTER*3 CODE
      CHARACTER*16 ELID(MSEC)
      CHARACTER*132 TBNAM,CDNAM,BLOT
      CHARACTER*10 ITOSTR

C
C     + + + EXTERNALS + + +
      EXTERNAL HYDIE,BLOT
      EXTERNAL ITOSTR
C
C     + + + FORMATS + + +
101   FORMAT(A)
702   FORMAT(A80)
700   FORMAT('SAC -USGS slope-area program Ver ',A5,34X,'page  0',/)
C
      ERR=0
C     set filst for shydie to print out slope pgm tables
      FILST=-2
      NID=NSEC
      IF(NID.GT.MSEC) NID=MSEC
      FTEMP(1)=HUNIT(2)
      FTEMP(2)=-10
      TBUNT=HUNIT(3)
      DO I=1,NID
        FLGS(I)=1
      END DO

      RTITLE = 'iRIC SAC Project'

C     In case launched from iRIC, result is output into STDOUT.
      FTEMP(2)=6
      OFILE=FTEMP(2)

      WRITE(OFILE,700) VERS
      WRITE(OFILE,'(A)')'Echo input data file '
      WRITE(OFILE,702) RTITLE
      
      CALL CG_OPEN_F('Case1.cgn',CG_MODE_MODIFY,CGNSID,IER)
      CALL CG_IRIC_INIT_F(CGNSID,IER)
      
C     read SI
      CALL CG_IRIC_READ_INTEGER_F('units',SI,IER)
      
      WRITE(0,101) 'SI        ' // TRIM(ITOSTR(SI))

C     read *TT record
      CALL CG_IRIC_READ_INTEGER_F('SLPCODE',SLPCODE_INT,IER)
      IF(SLPCODE_INT == 0) THEN
        SLPCODE=' '
      ELSE IF(SLPCODE_INT == 1) THEN
        SLPCODE='1'
      ENDIF

      CALL HYDIE_CGNS(TBUNT,FTEMP,NID,ELID,FLGS,SI,FILST)
      
      CALL CG_CLOSE_F(CGNSID,IER)
      REWIND(UNIT=TBUNT)

      RETURN
      END
