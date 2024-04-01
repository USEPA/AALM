PROGRAM Leg16
   ! New Fortran executable for the All-Ages Lead Model (AALM) 
   ! An update of the 1993 Leggett radioactive nuclide model 
   ! Written by WGG at ICF for EPA starting in May 2021
   ! Last revised by WGG on 22 September 2023
   IMPLICIT NONE
   !
   !  Front-end variables
   !
   CHARACTER(12), PARAMETER :: version = "Sep 22 2023 "
   INTEGER, PARAMETER :: N1 = 100                           ! Maximum number of input points per source
   INTEGER, PARAMETER :: RK = 8                             
   INTEGER, PARAMETER :: Num_PC=50, Num_PV=22, Num_Comp0=27, Num_Flow=55, Num_TS=47, Num_MB=7
   INTEGER, PARAMETER :: Yin=1, Yeat = 2, Yinhal=3, Ycomp=4, Yflow=5, Ybody=6, Yout=7
   ! SrcT and SrcTM1 indices for tracking fate of up to 18 separate sources in GI tract
   INTEGER, PARAMETER :: Ingest18=1, F18LungPlas=2, F18LungStom=3, Y18Stom=4, F18StomSI=5,           &
      Y18SI=6, F18SIPlas=7, F18SIULI=8, Y18ULI=9, F18ULILLI=10, Y18LLI=11, F18LLIFe=12,              &
      Y18Plas=13, Y18Fe=14, Diff18=15
   ! Ylung indices for tracking fate of 3 sources in lungs
   INTEGER, PARAMETER :: YLET=1, YLTB=2, YLalv=3, YLint=4, F3LTBLET=5, F3LalvLTB=6,                  &
      F3LalvLint=7, F3LETStom=8, F3LETPlas=9, F3LTBPlas=10, F3LalvPlas=11, F3LintPlas=12   
   ! PC parameters next (physiological constants)
   INTEGER, PARAMETER :: Ashwt=1, BldMot=2, BonIn=3, BranIn=4, Bratio=5, CrtWt=6, H1toBl=7,          &
      H1toH2=8, H1toSI=9, HepIn=10, IFetal=11, Kwt=12, Plsvol=13, Power=14, RBCIn=15, RBCnl=16,      &
      RBCVol=17, RenIn=18, RKdn1=19, RLLI=20, RLvr1=21, RPlas=22, RProt=23, RSIC=24, RSof0=25,       &
      RSof1=26, RSof2=27, RStmc=28, RULI=29, S2Hair=30, SatRat=31, SizeVF=32, SofIn=33, TBoneL=34,   &
      TEVF=35, ToFece=36, ToKdn1=37, ToKdn2=38, ToLvr1=39, ToProt=40, ToRBC=41, ToSwet=42,           &
      ToUrin=43, TrbWt=44, VBLC=45, VKC=46, VLC=47, VLUC=48, HCTA=49, HCTB=50
   ! PV vectors next (physiological variables that depend on age)
   INTEGER, PARAMETER :: AmtBld=1, F1=2, FLong=3, GScal=4, RBlad=5, RBran=6, RCort=7, Rcs2b=8,       &
      Rcs2df=9, RDiff=10, RKdn2=11, RLvr2=12, RRBC=13, RTrab=14, Rts2b=15, Rts2df=16, TBone=17,      &
      TFrac=18, ToBran=19, ToSof0=20, ToSof1=21, ToSof2=22 
   ! TS variables next (quantities reported on each timestep)
   INTEGER, PARAMETER :: Blood_vol=1, Body_wt=2, Bone_vol=3, Bone_wt=4, Cort_wt=5, F1_abs=6,         &
      Hematocrit=7, Kid_wt=8, Liv_wt=9, Plas_vol=10, RBC_conc=11, RBC_vol=12, R_Blad=13,             &
      R_Bran=14, R_CdifCsur=15, R_CdifCvol=16, R_Cort=17, R_CsurPlas=18, R_CsurCdif=19,              &
      R_EVF=20, R_Kdn2=21, R_Lvr2=22, R_Plas=23, R_RBC=24, R_TdifTsur=25, R_TdifTvol=26,             &
      R_Trab=27, R_TsurPlas=28, R_TsurTdif=29, Skel_wt=30, Trab_wt=31, T_Bone=32, T_Bran=33,         &
      T_EVF=34, T_Frac=35, T_Kdn1=36, T_Kdn2=37, T_Lvr1=38, T_Prot=39, T_RBC=40, T_SI=41,            &  
      T_Sof0=42, T_Sof1=43, T_Sof2=44, T_Sum=45, T_Swet=46, T_Urin=47 
   ! Compartments next
   INTEGER, PARAMETER :: Stom=1, SI=2, ULI=3, LLI=4, Csur=5, Cdif=6, Cvol=7, Tsur=8, Tdif=9,         &
      Tvol=10, Plas=11, Prot=12, RBC=13, Lvr1=14, Lvr2=15, Kdn1=16, Kdn2=17, Blad=18, Bran=19,       &
      EVF=20, Sof0=21, Sof1=22, Sof2=23, Urin=24, Fece=25, Swet=26, Hair=27, LET=28, LTB=29,         &
      Lalv=30, Lint=31
   CHARACTER(4), PARAMETER :: CompName(31) = (/"Stom", "SI  ", "ULI ", "LLI ", "Csur", "Cdif",       &
      "Cvol", "Tsur", "Tdif", "Tvol", "Plas", "Prot", "RBC ", "Lvr1", "Lvr2", "Kdn1", "Kdn2",        &
      "Blad", "Bran", "EVF ", "Sof0", "Sof1", "Sof2", "Urin", "Fece", "Swet", "Hair", "LET ",        &
      "LTB ", "Lalv", "Lint" /) 
   ! Inter-compartmental Flows next
   INTEGER, PARAMETER :: Lung_Plas=1, Lung_Stom=2, Stom_SI=3, SI_Plas=4, SI_ULI=5, ULI_LLI=6,        &
      LLI_Fece=7,   Csur_Plas=8,  Csur_Cdif=9,  Cdif_Csur=10, Cdif_Cvol=11, Cvol_Plas=12,            & 
      Tsur_Plas=13, Tsur_Tdif=14, Tdif_Tsur=15, Tdif_Tvol=16, Tvol_Plas=17, Lvr1_Lvr2=18,            &
      Lvr1_Plas=19, Lvr1_SI=20,   Lvr2_Plas=21, Kdn1_Blad=22, Kdn2_Plas=23, Blad_Urin=24,            &
      Bran_Plas=25, EVF_Plas=26,  Sof0_Plas=27, Sof1_Plas=28, Sof1_Hair=29, Sof2_Plas=30,            &
      Prot_Plas=31, RBC_Plas=32,  Plas_SI=33,   Plas_Prot=34, Plas_RBC=35,  Plas_EVF=36,             &
      Plas_Sof0=37, Plas_Sof1=38, Plas_Sof2=39, Plas_Bran=40, Plas_Csur=41, Plas_Tsur=42,            &
      Plas_Lvr1=43, Plas_Kdn1=44, Plas_Kdn2=45, Plas_Blad=46, Plas_Swet=47, LET_Plas=48,             &
      LET_Stom=49,  LTB_Plas=50,  LTB_LET=51,   Lalv_Plas=52, Lalv_LTB=53,  Lalv_Lint=54,            &
      Lint_Plas=55 
   CHARACTER(9), PARAMETER :: FlowName(Num_Flow) = (/"Lung_Plas", "Lung_Stom", "Stom_SI  ",          &
      "SI_Plas  ", "SI_ULI   ", "ULI_LLI  ", "LLI_Fece ", "Csur_Plas", "Csur_Cdif", "Cdif_Csur",     &
      "Cdif_Cvol", "Cvol_Plas", "Tsur_Plas", "Tsur_Tdif", "Tdif_Tsur", "Tdif_Tvol", "Tvol_Plas",     &
      "Lvr1_Lvr2", "Lvr1_Plas", "Lvr1_SI  ", "Lvr2_Plas", "Kdn1_Blad", "Kdn2_Plas", "Blad_Urin",     &
      "Bran_Plas", "EVF_Plas ", "Sof0_Plas", "Sof1_Plas", "Sof1_Hair", "Sof2_Plas", "Prot_Plas",     &
      "RBC_Plas ", "Plas_SI  ", "Plas_Prot", "Plas_RBC ", "Plas_EVF ", "Plas_Sof0", "Plas_Sof1",     &
      "Plas_Sof2", "Plas_Bran", "Plas_Csur", "Plas_Tsur", "Plas_Lvr1", "Plas_Kdn1", "Plas_Kdn2",     &
      "Plas_Blad", "Plas_Swet", "LET_Plas ", "LET_Stom ", "LTB_Plas ", "LTB_LET  ", "Lalv_Plas",     &
      "Lalv_LTB ", "Lalv_Lint", "Lint_Plas"/)
   ! Daily intakes and uptakes
   INTEGER, PARAMETER   :: InAirTot=1, InAirDep=2, InIngest=3, InDust=4, InSoil=5, InWater=6, InFood=7, InOther=8
   INTEGER, PARAMETER   :: UpTotal=1, UpAir=2, UpLung=3, UpGIair=4, UpGITotal=5, UpIngest=6,         &
       UpGIDust=7, UpGISoil=8, UpGIWater=9, UpGIFood=10, UpGIOther=11
   INTEGER, PARAMETER   :: ExAir=1, ExUrine=2, ExFeces=3, ExSweat=4, ExHair=5
   INTEGER, PARAMETER   :: Cblood=1, Cplas=2, Ckidney=3, Cliver=4, Ccort=5, Ctrab=6, Cbone=7
   INTEGER, ALLOCATABLE :: ts_age(:), indx_age(:), indx_full(:), indx_ts(:), daynum(:), indx_write(:) 
   REAL(8), ALLOCATABLE :: age_ts(:), buff8(:), source(:,:)
   REAL(RK),ALLOCATABLE :: conc(:,:), intake_frac(:,:), intake_tot(:), mask10(:), Cout(:,:) 
   REAL(RK),ALLOCATABLE :: PVarray(:,:), PVrow(:), full_ts(:), buff4(:), timeseries(:), agescl(:)
   REAL(RK)             :: Intakes(8) = 0., Uptakes(11) = 0., Excrete(5) = 0.
   REAL(RK)             :: PC(NUM_PC)=0., PVin(NUM_PV,N1)=0.                    ! Vectors for raw physiology data 
   REAL(RK)             :: delt, P, outrate                                     ! Timestep, inflow & outflow rates
   REAL(RK)             :: SrcT(15, 0:18), SrcTM1(15, 0:18)                     ! GI tract Pb by source   
   REAL(RK)             :: YlungT(12,3), YlungTM1(12,3), LungDep(3)             ! Lung Pb by source and total dep.
   REAL(RK)             :: scalar=0., lastscalar=0., nextscalar=0.              ! Multipliers for iterative option
   REAL(RK)             :: adjust(18) = 1.0,  lastadjust(18) = 1.0              ! Source-specific multiplier
   INTEGER              :: adjustable(18) = 0                                   ! Which sources are scalable
   INTEGER              :: I, J, K, iteration, line, pos1, pos2                 ! Looping and parsing variables
   INTEGER              :: ioerr, allocerr                                      ! Error status variables
   INTEGER              :: T                                                    ! Timestep number
   INTEGER              :: NTS=0                                                ! Number of timesteps in simulation
   INTEGER              :: Ndays=0                                              ! Number of days in simulation
   INTEGER              :: Nperday=0                                            ! Number of timesteps per day
   INTEGER              :: Nlung                                                ! Number of lung compartments
   INTEGER              :: nlev                                                 ! # levels in current input vector
   INTEGER              :: Nsource=18, Nbymedia(6)=0                             ! Source counts
   INTEGER              :: VI(0:N1)                                             ! Buffer for integer data
   REAL(RK)             :: VR(0:N1)                                             ! Buffer for real values
   CHARACTER(20)        :: var, runname                                         ! Input keyword
   CHARACTER(80)        :: fmt1, fmt2                                           ! Formats for output files
   CHARACTER(100)       :: ParFile, OutFile                                     ! Name of input and output files
   REAL(RK)             :: eat, ciliar, inhal, inmass, outmass, suppress
   REAL(RK)             :: cf, outplas, GIabs, dep(3), lungplas, eps=1E-9
   CHARACTER(5)         :: media(6) = (/"AIR  ","DUST ","SOIL ","WATER","FOOD ","OTHER"/)
   CHARACTER(3)         :: med3(6)  = (/"AIR","DUS","SOI","WAT","FOO","OTH"/)
   CHARACTER(3)         :: vartype
   CHARACTER(10)        :: DateTime(3) = ""
   INTEGER              :: DTValues(8) = 0
   INTEGER              :: SetAges(6) = (/100, 365, 1825, 3650, 5475, 9125/)
   INTEGER              :: WhichStep = 1 
   !
   TYPE :: LungType
      REAL(RK) :: DepFracLET  = 0.
      REAL(RK) :: DepFracLTB  = 0.
      REAL(RK) :: DepFracLAlv = 0.
      REAL(RK) :: RLETplas    = 0.
      REAL(RK) :: RLETstom    = 0.
      REAL(RK) :: RLTBplas    = 0.
      REAL(RK) :: RLTBLET     = 0.
      REAL(RK) :: RLalvPlas   = 0.
      REAL(RK) :: RLalvLTB    = 0.
      REAL(RK) :: RLalvLint   = 0.
      REAL(RK) :: RLintPlas   = 0.
   END TYPE Lungtype  
   TYPE(LungType) :: lung(3)
   !
   TYPE :: Growth
      CHARACTER(1) :: sex
      REAL(RK)      :: wbirth
      REAL(RK)      :: wchild
      REAL(RK)      :: half
      REAL(RK)      :: wadult
      REAL(RK)      :: kappa
      REAL(RK)      :: lambda
      REAL(RK)      :: lb
      REAL(RK)      :: hctb
      REAL(RK)      :: hcta
   END TYPE Growth
   TYPE(Growth) :: gr
   
   TYPE :: Iterate
      INTEGER      :: media
      INTEGER      :: subtype
      INTEGER      :: dustsoil  = 0
      REAL(RK)     :: targetBLL = 5.
      REAL(RK)     :: precision = 0.0001
      INTEGER      :: maxiter   = 6
      INTEGER      :: agewidth  = 32850
      INTEGER      :: agemin    = 0
      INTEGER      :: metric    = 0
      REAL(RK)     :: GSD       = 1.6
      REAL(RK)     :: tailfrac  = 0.05
   END TYPE Iterate
   TYPE(Iterate) :: iter
   !
   TYPE :: TargetBLL
      REAL(RK) :: lower
      REAL(RK) :: center
      REAL(RK) :: upper
      REAL(RK) :: previous
      REAL(RK) :: actual
   END TYPE TargetBLL
   TYPE(TargetBLL) :: BLL
   !
   TYPE :: Sourcetype
      CHARACTER(6) :: name   = ""
      CHARACTER(5) :: media  = ""
      INTEGER      :: nmedia = 0
      INTEGER      :: nsub   = 0
      REAL(8)      :: RBA    = 0.
   END TYPE SourceType
   Type(Sourcetype) :: AllSource(0:18)
   !
   TYPE :: Simtype 
      INTEGER :: agemin      = 0
      INTEGER :: agemax      = 32850
      INTEGER :: debug       = 0
      INTEGER :: iterate     = 0
      INTEGER :: outwrite    = 1
      INTEGER :: irbc        = 0
      INTEGER :: interp      = 0
   END TYPE Simtype
   TYPE(Simtype) :: sim
   !  Main loop variables
   REAL(RK), ALLOCATABLE :: Y(:,:)          ! Compartmental Pb mass on each timestep
   REAL(RK), ALLOCATABLE :: FL(:,:)         ! Intercompartmental flows
   REAL(RK), ALLOCATABLE :: MB(:,:)         ! Pb mass balance totals
   REAL(RK), ALLOCATABLE :: TS(:,:)         ! Time-varying modeling parameters
   REAL(RK), ALLOCATABLE :: NDEP(:,:)       ! Nondeposited air sources
   !
   !
   ! Executable statements
   !
   !
   CALL GET_COMMAND_ARGUMENT(1,ParFile)
   IF(ParFile=="") ParFile="LeggettInput.txt"
   OPEN(11, FILE=ParFile, IOSTAT=ioerr, ACTION="READ", RECL=1000)
   IF (ioerr<0) STOP
   READ(11,*,IOSTAT=ioerr) vartype
   DO WHILE(vartype(1:1) == "!")
      READ(11,*,IOSTAT=ioerr) vartype
   ENDDO   
   BACKSPACE(11)
   READ(11,*,IOSTAT=ioerr) vartype, runname
   
   OutFile = TRIM(ADJUSTL(runname))//"/RunInfo_"//TRIM(ADJUSTL(runname))//".txt"
   OPEN(15, FILE=Outfile, IOSTAT=ioerr, ACTION="WRITE", RECL=100)
   WRITE(15,*) "AALM Leggett Fortran code RunInfo file"
   CALL DATE_AND_TIME(DateTime(1),DateTime(2),DateTime(3),DTValues(1:8))
   WRITE(15,'(A15,7I8)') " Run at time = ", DTValues(1:3), DTValues(5:8)
   WRITE(15,*) "Run name = ", TRIM(ADJUSTL(runname))
   
   OutFile = TRIM(ADJUSTL(runname))//"/Log_"//TRIM(ADJUSTL(runname))//".csv"
   OPEN(12, FILE=OutFile, IOSTAT=ioerr, ACTION="WRITE", RECL=1000)
   OutFile = TRIM(ADJUSTL(runname))//"/Src_"//TRIM(ADJUSTL(runname))//".csv"
   OPEN(13, FILE=OutFile, IOSTAT=ioerr, ACTION="WRITE", RECL=1000)
   OutFile = TRIM(ADJUSTL(runname))//"/Out_"//TRIM(ADJUSTL(runname))//".csv"
   OPEN(14, FILE=OutFile, IOSTAT=ioerr, ACTION="WRITE", RECL=1000)
   OutFile = TRIM(ADJUSTL(runname))//"/Day_"//TRIM(ADJUSTL(runname))//".csv"
   OPEN(16, FILE=OutFile, IOSTAT=ioerr, ACTION="WRITE", RECL=1000)
   PRINT*, "OutFile = ", TRIM(ADJUSTL(OutFile))
   OutFile = TRIM(ADJUSTL(runname))//"/Rates_"//TRIM(ADJUSTL(runname))//".csv"
   OPEN(17, FILE=OutFile, IOSTAT=ioerr, ACTION="WRITE", RECL=1000)
   IF (ioerr<0) STOP
   PRINT*; PRINT*, " ***  AALM Fortran code, version ", version, "***"; PRINT* 
   PRINT*, "Input file = ", TRIM(ParFile)
   PRINT*, "Run name = ", runname; PRINT*
   DO line=1,200
      READ(11,*,IOSTAT=ioerr) vartype
      IF (vartype(1:1) == "!") CYCLE
      IF (UP(vartype(1:3)) == "END") EXIT
      BACKSPACE(11)
      READ(11,*,IOSTAT=ioerr) vartype, var, nlev
      BACKSPACE(11)
      READ(11,*,IOSTAT=ioerr) vartype, var, nlev, VR(1:nlev)
      VI(1:nlev) = NINT(VR(1:nlev))
      vartype = Up(vartype)
      var     = Up(var)
      IF (sim%debug==1) PRINT*, vartype, "  ", var, nlev, VR(1)
      IF (vartype=="SIM") CALL Read_sim_data()
      IF (vartype=="LUN") CALL Read_lung_data()
      IF (vartype=="GRO") CALL Read_growth_data()
      IF (vartype=="ITE") CALL Read_iter_data()
      IF (vartype=="PHY") CALL Read_phys_data()
      IF (ANY(med3==vartype)) CALL Read_media_data()
   ENDDO
   CLOSE(11)
   CALL InputSummary()
   SetAges = SetAges * Nperday
   IF(gr%sex=="M") WRITE(17, *) " Sex = Male"
   IF(gr%sex=="F") WRITE(17, *) " Sex = Female"
   WRITE(17, *) ""
   ! Create array to hold adjusted timeseries vectors
   ALLOCATE(TS(Num_TS,0:NTS), STAT=allocerr)
   CALL Adjust_inputs()
   DEALLOCATE(PVarray, timeseries, full_ts, conc, intake_frac, intake_tot, mask10)
   IF (sim%iterate==0)  iter%maxiter=1
   IF (sim%iterate==1)  CALL findtarget()
   !
   !
   !  Start of main Leggett code 
   !
   !
   ALLOCATE(Y(Num_Comp0 + 4, 0:NTS), STAT=allocerr)
   ALLOCATE(FL(Num_Flow, 0:NTS), STAT=allocerr)
   ALLOCATE(MB(Num_MB, 0:NTS), STAT=allocerr)
   ALLOCATE(NDEP(3, 0:NTS), STAT=allocerr)
   ALLOCATE(Cout(7,0:NTS), STAT=allocerr)
   DO iteration=1, iter%maxiter
      Y        = 0.
      FL       = 0.
      MB       = 0.
      SrcT     = 0.
      SrcTM1   = 0.
      YlungT   = 0.
      YlungTM1 = 0.
      ! Set scalable sources
      IF (sim%iterate==1) THEN
         IF (iteration==1) THEN
            scalar = 0.
            adjust(1:18) = 1.0 - REAL(adjustable(1:18))
            PRINT*, "adjust (step 1) = ", adjust(1:18)
         ENDIF    
         IF (iteration==2) THEN
            scalar = 1.
            lastscalar = 0.
            adjust(1:18) = 1.0
            PRINT*, "adjust (step 2) = ", adjust(1:18)
         ENDIF    
         IF (iteration>2) THEN
            nextscalar = scalar + (scalar-lastscalar)*(BLL%center-BLL%actual)/(BLL%actual-BLL%previous) 
            !adjust(1:18) = 1.0 + REAL(adjustable(1:18)) * (adjust(1:18) * nextscalar - 1.0)
            adjust(1:18) = 1.0 + REAL(adjustable(1:18)) * (nextscalar - 1.0)
            PRINT*, "lastscalar, scalar, nextscalar = "
            PRINT '(3F8.3)', lastscalar, scalar, nextscalar
            PRINT*, "adjust  = ", adjust(1:18)
            lastscalar = scalar
            scalar = nextscalar
         ENDIF 
      ENDIF    
      ! Set birth Pb if simulation starts at age 0
      IF(sim%agemin==0. .AND. PC(RBCin)>0.) THEN
         Y(RBC,  0) = PC(bldmot) * PC(bratio) * 3. 
         Y(Bran, 0) = Y(RBC, 0) * PC(BranIn) / PC(RBCIn)
         Y(Sof0, 0) = Y(RBC, 0) * PC(SofIn)  / PC(RBCIn)
         Y(Kdn2, 0) = Y(RBC, 0) * PC(RenIn)  / PC(RBCIn)
         Y(Lvr2, 0) = Y(RBC, 0) * PC(HepIn)  / PC(RBCIn)
         Y(Cvol, 0) = Y(RBC, 0) * PC(BonIn)  / PC(RBCIn) * 0.8
         Y(Tvol, 0) = Y(RBC, 0) * PC(BonIn)  / PC(RBCIn) * 0.2
         MB(YComp, 0) = Y(RBC,0)+Y(BRAN,0)+Y(SOF0,0)+Y(KDN2,0)+Y(LVR2,0)+Y(CVOL,0)+Y(TVOL,0)
         MB(Ybody, 0) = MB(YComp, 0) 
         IF (sim%debug==1) PRINT*, "Y0 = ", (Y(J,0), J=1,27)
      ENDIF
      MB(Yin, 0) = MB(Ycomp, 0)
      !
      !  Loop over timesteps
      !
      PRINT*, " Ingestion sources timestep 1 = ", source(4:Nsource, 1)
      PRINT*, " Adjustment on timestep 1     = ", adjust(1:Nsource)
      !PRINT*, "Non-linear parameters:"
      !PRINT*, PC(RBCnl), PC(SatRat), PC(power)
      !PRINT*, TS(Hematocrit, 1), TS(Blood_Vol, 1)
      !PRINT*, SUM(TS(Hematocrit, 1:NTS))/NTS, SUM(TS(Blood_Vol, 1:NTS))/NTS
      DO T=1, NTS
         ! First adjust TRBC for saturation effects
         IF(TS(Hematocrit, T)>0. .AND. TS(Blood_vol, T)>0.) THEN
            TS(RBC_conc, T) = Y(RBC, T-1) / (TS(Hematocrit, T-1) * TS(Blood_vol, T-1))
         ENDIF
         cf = 1.
         IF(sim%irbc==1 .AND. TS(RBC_conc, T-1) > PC(RBCnl)) THEN
            suppress = (1.-((TS(RBC_conc, T-1)-PC(RBCnl)) / (PC(SatRat)-PC(RBCnl))))**PC(power)
            IF(suppress<0.) suppress=0.
            cf = (TS(T_sum, T) - suppress* TS(T_RBC, T)) / (TS(T_sum, T) - TS(T_RBC, T))
            TS(T_RBC, T) = TS(T_RBC, T) * suppress
         ENDIF 
         !
         ! Stomach up to 18 sources (first 3 for air)
         SrcT(Ingest18, 1:3) = YLungTM1(F3LETStom, 1:3)
         SrcT(Ingest18, 4:Nsource) = source(4:Nsource, T) * adjust(4:Nsource)
         DO J = 1, NSource
            SrcT(Y18Stom, J) = Actvty(SrcTM1(Y18Stom, J), SrcT(Ingest18, J)/delt, PC(RStmc), delt)
            SrcT(F18StomSI, J) = SrcTM1(Y18Stom, J) + SrcT(Ingest18, J) - SrcT(Y18Stom, J)
         ENDDO
         eat            = SUM(SrcT(Ingest18,  4:Nsource))
         Y(Stom, T)     = SUM(SrcT(Y18Stom,   1:Nsource))
         FL(Stom_SI, T) = SUM(SrcT(F18StomSI, 1:NSource))
         !
         ! Small intestine up to 18 external sources (and 0 is for Liver + Plasma)
         SrcTM1(F18StomSI, 0) = FL(Lvr1_SI, T-1) + FL(Plas_SI, T-1)
         DO J = 0, NSource 
            SrcT(Y18SI, J) = Actvty(SrcTM1(Y18SI, J), SrcTM1(F18StomSI, J)/delt, PC(RSIC), delt)
            outmass        = SrcTM1(Y18SI, J) + SrcTM1(F18StomSI, J) - SrcT(Y18SI, J)
            GIabs          = TS(F1_abs, T) * AllSource(J)%RBA
            SrcT(F18SIPlas, J) = outmass * GIabs 
            SrcT(F18SIULI, J)  = outmass * (1D0 - GIabs) 
         ENDDO
         Y(SI, T)       = SUM(SrcT(Y18SI,     0:Nsource))
         FL(SI_Plas, T) = SUM(SrcT(F18SIPlas, 0:Nsource))
         FL(SI_ULI, T)  = SUM(SrcT(F18SIULI,  0:Nsource))
         !
         ! Upper large intestine (ULI)
         DO J = 0, NSource 
            SrcT(Y18ULI, J) = Actvty(SrcTM1(Y18ULI, J), SrcTM1(F18SIULI, J)/delt, PC(RULI), delt)
            SrcT(F18ULILLI, J) = SrcTM1(Y18ULI, J) + SrcTM1(F18SIULI, J) - SrcT(Y18ULI, J) 
         ENDDO
         Y(ULI, T)      = SUM(SrcT(Y18ULI,    0:Nsource))
         FL(ULI_LLI, T) = SUM(SrcT(F18ULILLI, 0:Nsource))
         !
         ! Lower large intestine (LLI)
         DO J = 0, NSource 
            SrcT(Y18LLI, J) = Actvty(SrcTM1(Y18LLI, J), SrcTM1(F18ULILLI, J)/delt, PC(RLLI), delt)
            SrcT(F18LLIFe, J) = SrcTM1(Y18LLI, J) + SrcTM1(F18ULILLI, J) - SrcT(Y18LLI, J) 
         ENDDO
         Y(LLI, T)       = SUM(SrcT(Y18LLI,   0:Nsource))
         FL(LLI_Fece, T) = SUM(SrcT(F18LLIFe, 0:Nsource))
         !
         ! Feces (Fece)
         DO J = 0, NSource 
            SrcT(Y18Fe, J)    = SrcTM1(Y18Fe, J) + SrcTM1(F18LLIFe, J)
         ENDDO    
         Y(Fece, T) = SUM(SrcT(Y18Fe, 0:Nsource))
         !
         ! Plasma (Plas)
         SrcT(Y18Plas, 0) = SrcTM1(Y18Plas, 0) + SrcTM1(F18SIPlas, 0)
         DO J = 1, 3
            SrcT(Y18Plas, J) = SrcTM1(Y18Plas, J) + YlungTM1(F3LETPlas, J) + YlungTM1(F3LTBPlas, J) +   &  
                               YlungTM1(F3LalvPlas, J) + YlungTM1(F3LintPlas, J) + SrcTM1(F18SIPlas, J)  
         ENDDO    
         DO J = 4, NSource 
            SrcT(Y18Plas, J) = SrcTM1(Y18Plas, J) + SrcTM1(F18SIPlas, J)     
         ENDDO   
         P = FL(Prot_Plas, T-1) + FL(RBC_Plas,  T-1) + FL(EVF_Plas,  T-1) + FL(Sof0_Plas, T-1) +    &
             FL(Sof1_Plas, T-1) + FL(Sof2_Plas, T-1) + FL(Lvr1_Plas, T-1) + FL(Lvr2_Plas, T-1) +    &
             FL(Kdn2_Plas, T-1) + FL(Csur_Plas, T-1) + FL(Tsur_Plas, T-1) + FL(Cvol_Plas, T-1) +    &
             FL(Tvol_Plas, T-1) + FL(Bran_Plas, T-1) + FL(SI_Plas,   T-1) + FL(LET_Plas, T-1)  +    &
             FL(LTB_Plas, T-1)  + FL(Lalv_Plas, T-1) + FL(Lint_Plas, T-1)
         Y(PLAS, T) = Actvty(Y(Plas, T-1), P/delt, TS(R_Plas, T), delt)
         outplas = (Y(Plas, T-1) + P - Y(Plas, T)) / TS(T_sum, T)
         FL(Plas_RBC,  T) = outplas * TS(T_RBC, T) 
         ! cf increases all the other flows when trbc is suppressed
         FL(Plas_Prot, T) = outplas * cf * TS(T_Prot, T) 
         FL(Plas_SI,   T) = outplas * cf * TS(T_SI,   T)
         FL(Plas_EVF,  T) = outplas * cf * TS(T_EVF,  T)  
         FL(Plas_Sof0, T) = outplas * cf * TS(T_Sof0, T)
         FL(Plas_Sof1, T) = outplas * cf * TS(T_Sof1, T)
         FL(Plas_Sof2, T) = outplas * cf * TS(T_Sof2, T)
         FL(Plas_Bran, T) = outplas * cf * TS(T_Bran, T)
         FL(Plas_Csur, T) = outplas * cf * TS(T_bone, T) * (1.-TS(T_Frac, T))
         FL(Plas_Tsur, T) = outplas * cf * TS(T_bone, T) * TS(T_Frac, T)
         FL(Plas_Lvr1, T) = outplas * cf * TS(T_Lvr1, T)
         FL(Plas_Kdn1, T) = outplas * cf * TS(T_Kdn1, T)
         FL(Plas_Kdn2, T) = outplas * cf * TS(T_Kdn2, T)
         FL(Plas_Blad, T) = outplas * cf * TS(T_Urin, T)
         FL(Plas_Swet, T) = outplas * cf * TS(T_Swet, T)
         !
         ! Plasma protein-bound (Prot)
         Y(Prot, T) = Actvty(Y(Prot, T-1), FL(Plas_Prot, T-1)/delt, PC(RProt), delt)
         FL(Prot_Plas, T) = Y(Prot, T-1) + FL(Plas_Prot, T-1) - Y(Prot, T)
         !
         ! Red blood cells (RBC)
         Y(RBC, T) = Actvty(Y(RBC, T-1), FL(Plas_RBC, T-1)/delt, TS(R_RBC, T), delt)
         FL(RBC_Plas, T) = Y(RBC, T-1) + FL(Plas_RBC, T-1) - Y(RBC, T)
         !
         ! Extra-vascular fluid (EVF)
         Y(EVF, T) = Actvty(Y(EVF, T-1), FL(Plas_EVF, T-1)/delt, TS(R_EVF, T), delt)
         FL(EVF_Plas, T) = Y(EVF, T-1) + FL(Plas_EVF, T-1) - Y(EVF, T)
         !
         ! Fast turnover soft tissue (Sof0)
         Y(Sof0, T) = Actvty(Y(Sof0, T-1), FL(Plas_Sof0, T-1)/delt, PC(RSof0), delt)
         FL(Sof0_Plas, T) = Y(Sof0, T-1) + FL(Plas_Sof0, T-1) - Y(Sof0, T)
         !
         ! Intermediate turnover soft tissue (Sof1)
         Y(Sof1, T) = Actvty(Y(Sof1, T-1), FL(Plas_Sof1, T-1)/delt, PC(RSof1), delt)
         outmass = Y(Sof1, T-1) + FL(Plas_Sof1, T-1) - Y(Sof1, T)
         FL(Sof1_Hair, T) = outmass * PC(S2Hair)
         FL(Sof1_Plas, T) = outmass * (1. - PC(S2Hair))
         !
         ! Slow turnover soft tissue (Sof2)
         Y(Sof2, T) = Actvty(Y(Sof2, T-1), FL(Plas_Sof2, T-1)/delt, PC(RSof2), delt)
         FL(Sof2_Plas, T) = Y(Sof2, T-1) + FL(Plas_Sof2, T-1) - Y(Sof2, T)
         !
         ! Brain (Bran)
         Y(Bran, T) = Actvty(Y(Bran, T-1), FL(Plas_Bran, T-1)/delt, TS(R_Bran, T), delt)
         FL(Bran_Plas, T) = Y(Bran, T-1) + FL(Plas_Bran, T-1) - Y(Bran, T)
         !
         ! Cortical surface (Csur)
         inmass  = FL(Plas_Csur, T-1) + FL(Cdif_Csur, T-1)
         outrate = TS(R_CsurPlas, T) + TS(R_CsurCdif, T)
         Y(Csur, T) = Actvty(Y(Csur, T-1), inmass/delt, outrate, delt)
         outmass = Y(Csur, T-1) + inmass - Y(Csur, T)
         IF(outrate>0.) THEN
            FL(Csur_Plas, T) = outmass * TS(R_CsurPlas, T) / outrate
            FL(Csur_Cdif, T) = outmass * TS(R_CsurCdif, T) / outrate
         ENDIF 
         !
         ! Exchangeable cortical volume (Cdif)
         outrate = TS(R_CdifCsur, T) + TS(R_CdifCvol, T)
         Y(Cdif, T) = Actvty(Y(Cdif, T-1), FL(Csur_Cdif, T-1)/delt, outrate, delt)
         outmass = Y(Cdif, T-1) + FL(Csur_Cdif, T-1) - Y(Cdif, T)
         IF(outrate>0.) THEN
            FL(Cdif_Csur, T) = outmass * TS(R_CdifCsur, T) / outrate
            FL(Cdif_Cvol, T) = outmass * TS(R_CdifCvol, T) / outrate
         ENDIF 
         !
         ! Nonexchangeable cortical volume (Cvol)
         Y(Cvol, T) = Actvty(Y(Cvol, T-1), FL(Cdif_Cvol, T-1)/delt, TS(R_Cort, T), delt)
         FL(Cvol_Plas, T) = Y(Cvol, T-1) + FL(Cdif_Cvol, T-1) - Y(Cvol, T)
         !
         ! Trabecular surface (Tsur)
         P = FL(Plas_Tsur, T-1) + FL(Tdif_Tsur, T-1)
         outrate = TS(R_TsurPlas, T) + TS(R_TsurTdif, T)
         Y(Tsur, T) = Actvty(Y(Tsur, T-1), P/delt, outrate, delt)
         outmass = Y(Tsur, T-1) + P - Y(Tsur, T)
         IF(outrate>0.) THEN
            FL(Tsur_Plas, T) = outmass * TS(R_TsurPlas, T) / outrate
            FL(Tsur_Tdif, T) = outmass * TS(R_TsurTdif, T) / outrate
         ENDIF 
         !
         ! Exchangeable trabecular volume (Tdif)
         outrate = TS(R_TdifTsur, T) + TS(R_TdifTvol, T)
         Y(Tdif, T) = Actvty(Y(Tdif, T-1), FL(Tsur_Tdif, T-1)/delt, outrate, delt)
         outmass = Y(Tdif, T-1) + FL(Tsur_Tdif, T-1) - Y(Tdif, T)
         IF(outrate>0.) THEN
            FL(Tdif_Tsur, T) = outmass * TS(R_TdifTsur, T) / outrate
            FL(Tdif_Tvol, T) = outmass * TS(R_TdifTvol, T) / outrate
         ENDIF 
         !
         ! Nonexchangeable trabecular volume (Tvol)
         Y(Tvol, T) = Actvty(Y(Tvol, T-1), FL(Tdif_Tvol, T-1)/delt, TS(R_Trab, T), delt)
         FL(Tvol_Plas, T) = Y(Tvol, T-1) + FL(Tdif_Tvol, T-1) - Y(Tvol, T)
         !
         ! Liver 1 (Lvr1)
         Y(Lvr1, T) = Actvty(Y(Lvr1, T-1), FL(Plas_Lvr1, T-1)/delt, PC(RLvr1), delt)
         outmass = Y(Lvr1, T-1) + FL(Plas_Lvr1, T-1) - Y(Lvr1, T)
         FL(Lvr1_Lvr2, T) = outmass * PC(H1toH2)
         FL(Lvr1_Plas, T) = outmass * PC(H1toBl)
         FL(Lvr1_SI,   T) = outmass * PC(H1toSI)
         !
         ! Liver 2 (Lvr2)
         Y(Lvr2, T) = Actvty(Y(Lvr2, T-1), FL(Lvr1_Lvr2, T-1)/delt, TS(R_Lvr2, T), delt)
         FL(Lvr2_Plas, T) = Y(Lvr2, T-1) + FL(Lvr1_Lvr2, T-1) - Y(Lvr2, T)
         !
         ! Kidneys 1 (Kdn1)
         Y(Kdn1, T) = Actvty(Y(Kdn1, T-1), FL(Plas_Kdn1, T-1)/delt, PC(RKdn1), delt)
         FL(Kdn1_Blad, T) = Y(Kdn1, T-1) + FL(Plas_Kdn1, T-1) - Y(Kdn1, T)
         !      
         ! Kidneys 2 (Kdn2)
         Y(Kdn2, T) = Actvty(Y(Kdn2, T-1), FL(Plas_Kdn2, T-1)/delt, TS(R_Kdn2, T), delt)
         FL(Kdn2_Plas, T) = Y(Kdn2, T-1) + FL(Plas_Kdn2, T-1) - Y(Kdn2, T) 
         !
         ! Bladder (Blad)
         inmass = FL(Kdn1_Blad, T-1) + FL(Plas_Blad, T-1)
         Y(Blad, T) = Actvty(Y(Blad, T-1), inmass/delt, TS(R_Blad, T), delt)
         FL(Blad_Urin, T) = Y(Blad, T-1) + inmass - Y(Blad, T)
         ! 
         ! Urine (Urin)
         Y(Urin, T) = Y(Urin, T-1) + FL(Blad_Urin, T-1)
         !
         ! Sweat (Swet)
         Y(Swet, T) = Y(Swet, T-1) + FL(Plas_Swet, T-1)
         !
         ! Hair (Hair)
         Y(Hair, T) = Y(Hair, T-1) + FL(Sof1_Hair, T-1)
         !
         ! Lungs Extrathoracic (LET or Ylung(1))
         dep   = 0.
         DO K=1, 3
            inhal   = source(K, T) * adjust(K) * Lung(K)%DepFracLET 
            dep(K)  = dep(K) + inhal
            inmass  = inhal + YlungTM1(F3LTBLET, K)
            outrate = Lung(K)%RLETplas + Lung(K)%RLETstom
            YLungT(YLET, K) = Actvty(YlungTM1(YLET, K), inmass/delt, outrate, delt)
            outmass = YLungTM1(YLET, K) + inmass - YLungT(YLET, K) 
            IF (outrate>0.) THEN
               YLungT(F3LETPlas, K) = outmass*Lung(K)%RLETplas/outrate
               YLungT(F3LETStom, K) = outmass*Lung(K)%RLETstom/outrate
            ENDIF   
         ENDDO
         Y(LET, T)       = SUM(YLungT(YLET, 1:3))
         FL(LET_Plas, T) = SUM(YLungT(F3LETPlas, 1:3))
         FL(LET_Stom, T) = SUM(YLungT(F3LETStom, 1:3))
         !
         ! Lungs Tracheo-broncular (LTB)
         DO K=1, 3
            inhal   = source(K, T) * adjust(K) * Lung(K)%DepFracLTB 
            dep(K)  = dep(K) + inhal
            inmass  = inhal + YLungTM1(F3LalvLTB, K)
            outrate = Lung(K)%RLTBplas + Lung(K)%RLTBLET
            YLungT(YLTB, K) = Actvty(YlungTM1(YLTB, K), inmass/delt, outrate, delt)
            outmass = YLungTM1(YLTB, K) + inmass - YLungT(YLTB, K) 
            IF (outrate>0.) THEN
               YLungT(F3LTBPlas, K) = outmass*Lung(K)%RLTBplas/outrate
               YLungT(F3LTBLET,  K) = outmass*Lung(K)%RLTBLET /outrate
            ENDIF   
         ENDDO
         Y(LTB, T)       = SUM(YLungT(YLTB, 1:3))
         FL(LTB_Plas, T) = SUM(YlungT(F3LTBPlas, 1:3))
         FL(LTB_LET,  T) = SUM(YLungT(F3LTBLET,  1:3))
         !
         ! Lungs Alveolar (Lalv)
         DO K=1, 3
            inhal   = source(K, T) * adjust(K) * Lung(K)%DepFracLalv
            dep(K)  = dep(K) + inhal
            inmass  = inhal
            outrate = Lung(K)%RLalvPlas + Lung(K)%RLalvLTB + Lung(K)%RLalvLint
            YLungT(YLalv, K) = Actvty(YlungTM1(3, K), inmass/delt, outrate, delt)
            outmass = YLungTM1(YLalv, K) + inmass - YLungT(YLalv, K) 
            IF (outrate>0.) THEN
               YLungT(F3LalvPlas, K) = outmass*Lung(K)%RLalvPlas/outrate
               YLungT(F3LalvLTB,  K) = outmass*Lung(K)%RLalvLTB /outrate
               YLungT(F3LalvLint, K) = outmass*Lung(K)%RLalvLint/outrate
            ENDIF   
         ENDDO
         Y(Lalv, T)       = SUM(YLungT(YLalv, 1:3))
         FL(Lalv_Plas, T) = SUM(YlungT(F3LalvPlas, 1:3))
         FL(Lalv_LTB,  T) = SUM(YlungT(F3LalvLTB,  1:3))
         FL(Lalv_Lint, T) = SUM(YlungT(F3LalvLint, 1:3))
         !
         ! Lungs Interstitial (Lint)
         DO K=1, 3
            inmass = YlungTM1(F3LalvLint, K)
            outrate = Lung(K)%RLintPlas
            YLungT(YLint, K) = Actvty(YlungTM1(YLint, K), inmass/delt, outrate, delt)
            YLungT(F3LintPlas, K) = YLungTM1(YLint, K) + inmass - YLungT(YLint, K) 
         ENDDO
         Y(Lint, T)       = SUM(YLungT(YLint, 1:3))
         FL(Lint_Plas, T) = SUM(YlungT(F3LintPlas, 1:3))
         !
         ! Lungs source and deposition
         NDEP(1:3, T) = source(1:3, T) - dep(1:3) 
         !PRINT*, " Lung sources = ", source(1:3, T)   
         !PRINT*, " Lung deposit = ", dep(1:3) 
         !PRINT*, " Lung nondep  = ", NDEP(1:3, T); PRINT*
         !
         ! Write out rates on selected timesteps
         IF(T .EQ. SetAges(WhichStep)) THEN 
             WRITE(17,*) "Timestep=",T, "  Year=", REAL(T)/REAL((365*Nperday))
             WRITE(17,*) "  Plasma-D to EVF        = ", TS(R_Plas, T) * TS(T_EVF,  T) / TS(T_Sum,  T)
             WRITE(17,*) "  Plasma-D to RBC        = ", TS(R_Plas, T) * TS(T_RBC,  T) / TS(T_Sum,  T)
             WRITE(17,*) "  Plasma-D to Plasma-B   = ", TS(R_Plas, T) * TS(T_Prot, T) / TS(T_Sum,  T)
             WRITE(17,*) "  Plasma-D to Bladder    = ", TS(R_Plas, T) * TS(T_Urin, T) / TS(T_Sum,  T)
             WRITE(17,*) "  Plasma-D to SI         = ", TS(R_Plas, T) * TS(T_SI,   T) / TS(T_Sum,  T)
             WRITE(17,*) "  Plasma-D to Trab surf  = ", TS(R_Plas, T) * TS(T_Bone, T) / TS(T_Sum,  T) * TS(T_Frac, T)
             WRITE(17,*) "  Plasma-D to Cort surf  = ", TS(R_Plas, T) * TS(T_Bone, T) / TS(T_Sum,  T) * (1-TS(T_Frac, T))
             WRITE(17,*) "  Plasma-D to Liver1     = ", TS(R_Plas, T) * TS(T_Lvr1, T) / TS(T_Sum,  T)
             WRITE(17,*) "  Plasma-D to Kidney1    = ", TS(R_Plas, T) * TS(T_Kdn1, T) / TS(T_Sum,  T)
             WRITE(17,*) "  Plasma-D to Kidney2    = ", TS(R_Plas, T) * TS(T_Kdn2, T) / TS(T_Sum,  T)
             WRITE(17,*) "  Plasma-D to Soft0      = ", TS(R_Plas, T) * TS(T_Sof0, T) / TS(T_Sum,  T)
             WRITE(17,*) "  Plasma-D to Soft1      = ", TS(R_Plas, T) * TS(T_Sof1, T) / TS(T_Sum,  T)
             WRITE(17,*) "  Plasma-D to Soft2      = ", TS(R_Plas, T) * TS(T_Sof2, T) / TS(T_Sum,  T)
             WRITE(17,*) "  Plasma-D to Brain      = ", TS(R_Plas, T) * TS(T_Bran, T) / TS(T_Sum,  T)
             WRITE(17,*) "  Plasma-D to Sweat      = ", TS(R_Plas, T) * TS(T_Swet, T) / TS(T_Sum,  T)
             WRITE(17,*) "  RBC to Plasma-D        = ", TS(R_RBC,  T)
             WRITE(17,*) "  EVF to Plasma-D        = ", TS(R_EVF,  T)
             WRITE(17,*) "  Plasma-B to Plasma-D   = ", PC(RProt)
             WRITE(17,*) "  Cort Surf to Plasma-D  = ", TS(R_CsurPlas, T)
             WRITE(17,*) "  Trab Surf to Plasma-D  = ", TS(R_TsurPlas, T)
             WRITE(17,*) "  Cort Surf to Exch Vol  = ", TS(R_CsurCdif, T)
             WRITE(17,*) "  Trab Surf to Exch Vol  = ", TS(R_TsurTdif, T)
             WRITE(17,*) "  Cort Exch Vol to Surf  = ", TS(R_CdifCsur, T)
             WRITE(17,*) "  Trab Exch Vol to Surf  = ", TS(R_TdifTsur, T)
             WRITE(17,*) "  Cort Exch Vol to NonEx = ", TS(R_CdifCvol, T)
             WRITE(17,*) "  Trab Exch Vol to Nonex = ", TS(R_TdifTvol, T)
             WRITE(17,*) "  Cort NonEx to Plasma-D = ", TS(R_Cort, T)
             WRITE(17,*) "  Trab NonEx to Plasma-D = ", TS(R_Trab, T)
             WRITE(17,*) "  Liver1 to Plasma_D     = ", PC(RLvr1) * PC(H1toBL)
             WRITE(17,*) "  Liver1 to SI           = ", PC(RLvr1) * PC(H1toSI)
             WRITE(17,*) "  Liver1 to Liver2       = ", PC(RLvr1) * PC(H1toH2)
             WRITE(17,*) "  Liver2 to Plasma-D     = ", TS(R_Lvr2, T)
             WRITE(17,*) "  Kidney1 to Bladder     = ", PC(RKdn1) 
             WRITE(17,*) "  Kidney2 to Plasma-D    = ", TS(R_Kdn2, T)
             WRITE(17,*) "  Soft0 to Plasma-D      = ", PC(RSOF0)
             WRITE(17,*) "  Soft1 to Plasma-D      = ", PC(RSOF1)*(1.-PC(S2Hair))
             WRITE(17,*) "  Soft1 to Hair          = ", PC(RSOF1)*PC(S2Hair)
             WRITE(17,*) "  Soft2 to Plasma-D      = ", PC(RSOF2)
             WRITE(17,*) "  Brain to Plasma-D      = ", TS(R_Bran, T)
             WRITE(17,*) " "
             WRITE(17,*) "  TSum                   = ", TS(T_Sum, T)
             WRITE(17,*) "  AgeScl                 = ", AgeScl(T)
             WRITE(17,*) "  Rplas                  = ", PC(RPlas)
             WRITE(17,*) "  Rpls                   = ", TS(R_Plas, T)
             WRITE(17,*) " "
             PRINT*, " T, Rbran = ", T, TS(R_Bran, T)
             PRINT*, " T, ToRBC = ", T, TS(T_RBC, T)
             WhichStep = WhichStep+1
         ENDIF    
         ! Daily intakes and uptakes
         Call IntakeUptake()
         !      
         ! Mass balance calculations 
         !
         MB(Yeat,  T) = eat
         MB(Yinhal,T) = SUM(dep(1:3))
         MB(Yin,   T) = MB(Yin, T-1) + MB(Yeat,  T) + MB(Yinhal,T)
         MB(Ycomp, T) = SUM(Y(:,T))
         MB(Yflow, T) = SUM(FL(:,T))
         MB(Yout,  T) = Y(Urin,T) + Y(Fece,T) + Y(Swet,T) + Y(Hair,T)
         MB(Ybody, T) = MB(Ycomp, T) + MB(Yflow, T) - MB(Yout, T)
         IF ((sim%debug==1 .AND. NTS<20*sim%outwrite) .OR. T==NTS) THEN
            PRINT*, "Timestep ", T
            DO J=1, 31
               PRINT*, "Y =", J, CompName(J), Y(J, T)
            ENDDO
            DO J=3, Num_flow
               PRINT*, "FL=", J, FlowName(J), FL(J, T) 
            ENDDO 
            PRINT*; PRINT*, "End of simulation Pb mass totals by source"
            DO J=1, Nbymedia(1)
               inmass   = SUM(Source(J, 1:T)*adjust(J)) - SUM(NDEP(J, 1:T))
               lungplas = 0. !SUM(YlungTM1(9:12, J))
               outmass  = SUM(SrcT(4:14, J)) + SUM(YlungT(1:12, J)) + lungplas
               PRINT '(1X,A6,A9,F14.3)', Allsource(J)%name, " Source =", SUM(Source(J, 1:T)*adjust(J))
               PRINT '(1X,A6,A9,F14.3)', Allsource(J)%name, " NonDep =", SUM(NDEP(J, 1:T))
               PRINT '(1X,A6,A9,F14.3)', Allsource(J)%name, " Intake =", inmass
               PRINT '(1X,A6,A9,F14.3)', Allsource(J)%name, " GItract=", SUM(SrcT(4:12, J))
               PRINT '(1X,A6,A9,F14.3)', Allsource(J)%name, " Lungs  =", SUM(YlungT(1:12, J))
               PRINT '(1X,A6,A9,F14.3)', Allsource(J)%name, " Absorb =", SrcT(Y18Plas, J) 
               PRINT '(1X,A6,A9,F14.3)', Allsource(J)%name, " Elim   =", SrcT(Y18Fe, J)
               PRINT '(1X,A6,A9,F14.3)', Allsource(J)%name, " Sum    =", outmass
               PRINT '(1X,A6,A9,F14.3)', Allsource(J)%name, " Diff   =", outmass - inmass
               PRINT*
            ENDDO    
            DO J=4, Nsource
               inmass  = SUM(Source(J, 1:T)*adjust(J))
               outmass = SUM(SrcT(2:14, J))
               PRINT '(1X,A6,A9,F14.3)', Allsource(J)%name, " Intake =", inmass
               PRINT '(1X,A6,A9,F14.3)', Allsource(J)%name, " GItract=", SUM(SrcT(4:12, J))
               PRINT '(1X,A6,A9,F14.3)', Allsource(J)%name, " Absorb =", SrcT(Y18Plas, J)
               PRINT '(1X,A6,A9,F14.3)', Allsource(J)%name, " Elim   =", SrcT(Y18Fe, J)
               PRINT '(1X,A6,A9,F14.3)', Allsource(J)%name, " Sum    =", outmass
               PRINT '(1X,A6,A9,F14.3)', Allsource(J)%name, " Diff   =", outmass - inmass
               PRINT*
            ENDDO    
            PRINT*; PRINT*, "Overall mass balance on step ", T
            PRINT '(A9,F16.3)', "Start  =", MB(Ycomp, 0)
            PRINT '(A9,F16.3)', "Intake =", MB(Yin,   T) - MB(Ycomp, 0)
            PRINT '(A9,F16.3)', "Sum    =", MB(Yin,   T); PRINT*
            PRINT '(A9,F16.3)', "Body   =", MB(Ybody, T) - MB(Yflow, T)
            PRINT '(A9,F16.3)', "Flows  =", MB(Yflow, T)
            PRINT '(A9,F16.3)', "Elim   =", MB(Yout,  T)
            PRINT '(A9,F16.3)', "Sum    =", MB(Ycomp, T) + MB(Yflow, T); PRINT*
            PRINT '(A9,F16.3)', "Diff   =", MB(Ybody, T) + MB(Yout, T) - MB(Yin, T)
            PRINT*
         ENDIF
         SrcTM1   = SrcT
         SrcT     = 0.
         YLungTM1 = YLungT
         YLungT   = 0.
      ENDDO              ! End loop over timesteps
      Cout(Cblood, 0:NTS) = (Y(Plas, 0:NTS)+Y(Prot, 0:NTS)+Y(RBC, 0:NTS)) / TS(Blood_vol, 0:NTS)
      Cout(Cplas,  0:NTS) = (Y(Plas, 0:NTS)+Y(Prot, 0:NTS)) / TS(Plas_vol, 0:NTS)
      Cout(Ckidney,0:NTS) = (Y(Kdn1, 0:NTS)+Y(Kdn2, 0:NTS)) / TS(Kid_wt, 0:NTS)
      Cout(Cliver, 0:NTS) = (Y(Lvr1, 0:NTS)+Y(Lvr2, 0:NTS)) / TS(Liv_wt, 0:NTS)
      Cout(Ccort,  0:NTS) = (Y(Csur, 0:NTS)+Y(Cdif, 0:NTS)+Y(Cvol, 0:NTS)) / TS(Cort_wt, 0:NTS)
      Cout(Ctrab,  0:NTS) = (Y(Tsur, 0:NTS)+Y(Tdif, 0:NTS)+Y(Tvol, 0:NTS)) / TS(Trab_wt, 0:NTS)
      Cout(Cbone,  0:NTS) = SUM(Y(Csur:Tvol,0:NTS),DIM=1)/TS(Bone_wt,0:NTS)
      PRINT '(A30,F8.3)',"Average BLL over simulation = ", SUM(Cout(Cblood, 0:NTS)) / REAL(NTS+1,RK); PRINT*
      IF (sim%iterate==1) THEN
         CALL Iteration_output()
         IF(iteration>2 .AND. BLL%lower<=BLL%actual .AND. BLL%actual<=BLL%upper) EXIT
      ENDIF
   ENDDO                 ! End loop over iterations 
   !
   ! Summarize data for output
   !
   PRINT*
   fmt1 = '(I8,24(A1,F15.5))'  
   WRITE(13,*) " Timestep, Days, Years, Air1, Air2, Air3, Dust1, Dust2, Dust3, Soil1, Soil2, Soil3, Water1, Water2, "//   &
               "Water3, Food1, Food2, Food3, Other1, Other2, Other3, Tintake, Tbody, Telim, Diff"
   DO J=sim%outwrite,NTS,sim%outwrite
      WRITE(13,fmt1) J, ",", age_ts(J), ",",age_ts(J)/365.,",", source(1,J)*adjust(1),",",source(2,J)*adjust(2),",",      &
         source(3,J)*adjust(3),",",source(4,J)*adjust(4),",",source(5,J)*adjust(5),",",source(6,J)*adjust(6),",",         &
         source(7,J)*adjust(7),",",source(8,J)*adjust(8),",",source(9,J)*adjust(9),",",source(10,J)*adjust(10),",",       &
         source(11,J)*adjust(11),",",source(12,J)*adjust(12),",",source(13,J)*adjust(13),",",source(14,J)*adjust(14),",", &   
         source(15,J)*adjust(15),",",source(16,J)*adjust(16),",",source(17,J)*adjust(17),",",source(18,J)*adjust(18),",", &
         MB(Yin,J),",", MB(Ybody,J),",",MB(Yout,J),",",MB(Ycomp,J)+MB(Yflow,J)-MB(Yin,J) 
   ENDDO   
   fmt2 =  '(I8,31(A1,F15.5))' 
   WRITE(14,*) " Timestep, Days, Years, Cblood, Cplas, Ckidney, Cliver, Ccort, Ctrab, Cbone, Ablood, Aplas, ARBC, Akidney" &
             //", Aliver, Acort, Atrab, Abone, Asoft, Abrain, ART, Astom, AGI, Aprot, AEVF, Ablad, Aflow, Tbody, Aurine,"  &
             //" Afecal, Asweat, Ahair"
   DO J=0,NTS,sim%outwrite
      WRITE(14,fmt2) J, ",", age_ts(J), ",",age_ts(J)/365.,",", Cout(Cblood,J),",",Cout(Cplas,J),",",Cout(Ckidney,J),",",  &
         Cout(Cliver,J),",",Cout(Ccort,J),",",Cout(Ctrab,J),",",Cout(Cbone,J),",",Y(Plas,J)+Y(RBC,J),",",Y(Plas,J),",",    &
         Y(RBC,J),",",Y(Kdn1,J)+Y(Kdn2,J),",",Y(Lvr1,J)+Y(Lvr2,J),",",Y(Csur,J)+Y(Cdif,J)+Y(Cvol,J),",",Y(Tsur,J)+         &
         Y(Tdif,J)+Y(Tvol,J),",",SUM(Y(Csur:Tvol,J)),",",SUM(Y(Sof0:Sof2,J)),",",Y(Bran,J),",",SUM(Y(LET:Lint,J))          &
         ,",",Y(Stom,J),",",SUM(Y(SI:LLI,J)),",",Y(Prot,J),",",Y(EVF,J),",",Y(Blad,J),",",MB(Yflow,J),",",MB(Ybody,J),     &
         ",",Y(Urin,J),",",Y(Fece,J),",",Y(Swet,J),",",Y(Hair,J) 
   ENDDO
   IF (sim%iterate==1) THEN
      WRITE(15, *) "Adjustment factors = "
      DO J=1,Nsource
         IF(adjust(J).NE. 1.0) WRITE(15, '(1X,A6,2X,F15.5)') Allsource(J)%name, adjust(J)
      ENDDO   
   ENDIF 
   WRITE(15,*) "Run successfully completed"
   CLOSE(12)                                 ! 12 = Log_    output file
   CLOSE(13)                                 ! 13 = Src_    output file
   CLOSE(14)                                 ! 14 = Out_    output file
   CLOSE(15)                                 ! 15 = RunInfo output file
   CLOSE(16)                                 ! 16 = Day_    output file
   CLOSE(17)                                 ! 17 = Rates   output file 
   STOP

   
CONTAINS
   
    
CHARACTER (LEN=20) FUNCTION Up(String)   
   CHARACTER (LEN=*), INTENT(IN) :: String 
   INTEGER :: I, J
   Up=""
   DO I=1, LEN_TRIM(String) 
      J = IACHAR(String(I:I))
      IF (J>96.AND.J<123) J=J-32
      Up(I:I) = ACHAR(J)
   ENDDO
END FUNCTION Up


CHARACTER (LEN=1000) FUNCTION CommaBlank(String)
   CHARACTER(LEN=1000), INTENT(IN) :: String
   INTEGER :: I, J
   CommaBlank = ADJUSTL(String)
   PRINT*, "input ", TRIM(CommaBlank)
   DO I = 1, LEN_TRIM(CommaBlank)
      IF(CommaBlank(I:I)==" ") THEN 
         CommaBlank(I:) = CommaBlank(I+1:)
      ENDIF   
   ENDDO  
   PRINT*, "during ", TRIM(CommaBlank)
   J = LEN_TRIM(CommaBlank)
   DO I = J, 1, -1
     IF(CommaBlank(I:I).NE."," ) RETURN
     CommaBlank(I:I) = " "
   ENDDO    
END FUNCTION CommaBlank


SUBROUTINE Read_sim_data()
   INTEGER :: N, agemin=0, agemax=90
   IF (var=="OUTWRITE") sim%outwrite = VI(1) 
   IF (var=="DEBUG")       sim%debug = VI(1)
   IF (var=="ITERATE")   sim%iterate = VI(1)
   IF (var=="INTERP")     sim%interp = VI(1)
   IF (var=="IRBC")         sim%irbc = VI(1)
   IF (var=="AGE_RANGE") THEN
      agemin = VI(1)
      agemax = VI(2)
      sim%agemin = agemin
      sim%agemax = agemax
      Ndays      = agemax - agemin
      PRINT*, "Number of days      = ", Ndays
   ENDIF
   IF (var=="STEPS_PER_DAY") THEN
      Nperday = VI(1)
      delt = REAL(1.0 / Nperday)
      PRINT*, "Timesteps per day   = ", Nperday
   ENDIF   
   IF (Ndays>0 .AND. Nperday>0 .AND. NTS==0 .AND. sim%outwrite>=0) THEN
      NTS  = Ndays * Nperday
      PRINT*, "Number of timesteps = ", NTS
      ! full_ts is used for interpolation outside age_range
      ALLOCATE(full_ts(0:36500*Nperday), STAT=allocerr)  
      ALLOCATE(indx_ts(0:NTS), STAT=allocerr)
      indx_ts = (/ (I, I = 0, NTS) /)
      ALLOCATE(indx_age(agemin:agemax), STAT=allocerr)
      indx_age = (/ (I, I = agemin, agemax) /)
      ALLOCATE(ts_age(agemin:agemax), STAT=allocerr)
      ts_age = (indx_age(:)-agemin)*Nperday
      ALLOCATE(age_ts(0:NTS), STAT=allocerr)
      age_ts = REAL(Nperday*sim%agemin + indx_ts(:),8) / REAL(Nperday,8)
      ALLOCATE(indx_full(0:(36500*Nperday)), STAT=allocerr)
      indx_full = (/ (I, I=0,36500*Nperday) /)
      ALLOCATE(buff4(1:NTS), STAT=allocerr)
      IF(sim%debug>0) THEN
         N = NTS/sim%outwrite
         ALLOCATE(indx_write(1:N), STAT=allocerr)
         ALLOCATE(buff8(1:N), STAT=allocerr)
         indx_write = indx_full(1:N)*sim%outwrite
         WRITE(12,*) "Age at end of timesteps  = "
         buff8 = age_ts(indx_write)
         WRITE(12,'(10F13.3)') buff8
         
      ENDIF
      ! arrays for holding media and phys data
      ALLOCATE(conc(3,0:NTS), STAT=allocerr)
      ALLOCATE(Intake_tot(0:NTS), STAT=allocerr)
      ALLOCATE(Intake_frac(3,0:NTS), STAT=allocerr)
      ALLOCATE(source(18,1:NTS), STAT=allocerr)
      ALLOCATE(timeseries(0:36500*Nperday), STAT=allocerr)
      ALLOCATE(daynum(1:NTS), STAT=allocerr)
      ALLOCATE(mask10(1:NTS), STAT=allocerr)
      ALLOCATE(agescl(0:NTS), STAT=allocerr)
      conc        = 0.
      intake_tot  = 0.
      intake_frac = 0.
      source      = 0.
      timeseries  = 0.
      daynum      = 0
      mask10      = 0.
      agescl      = 0.
   ENDIF 
END SUBROUTINE Read_sim_data  


SUBROUTINE Read_growth_data()
   IF(INDEX(var, "SEX")>0) THEN
      IF(VI(1)==0) gr%sex = "F"
      IF(VI(1)==1) gr%sex = "M"
   ENDIF
   IF(INDEX(var, "WBIRTH")>0) gr%wbirth = VR(1)
   IF(INDEX(var, "WCHILD")>0) gr%wchild = VR(1)
   IF(INDEX(var, "HALF")>0)   gr%half   = VR(1)
   IF(INDEX(var, "WADULT")>0) gr%wadult = VR(1)
   IF(INDEX(var, "KAPPA")>0)  gr%kappa  = VR(1)
   IF(INDEX(var, "LAMBDA")>0) gr%lambda = VR(1)
   IF(INDEX(var, "LB")>0)     gr%lb     = VR(1)
   IF(INDEX(var, "HCTB")>0)   gr%hctb   = VR(1)  
   IF(INDEX(var, "HCTA")>0)   gr%hcta   = VR(1)
END SUBROUTINE Read_growth_data
   

SUBROUTINE Read_lung_data()
   CHARACTER(1) :: nchar
   INTEGER :: nsrc
   nsrc = nlev
   IF(INDEX(var, "DEPFRACLET")>0) lung(1:nlev)%DepFracLET  = VR(1:nlev)
   IF(INDEX(var, "DEPFRACLTB")>0) lung(1:nlev)%DepFracLTB  = VR(1:nlev)
   IF(INDEX(var, "DEPFRACLA")>0)  lung(1:nlev)%DepFracLalv = VR(1:nlev)
   IF(INDEX(var, "RLETPLAS")>0)   lung(1:nlev)%RLETplas    = VR(1:nlev)
   IF(INDEX(var, "RLETSTOM")>0)   lung(1:nlev)%RLETstom    = VR(1:nlev)
   IF(INDEX(var, "RLTBPLAS")>0)   lung(1:nlev)%RLTBplas    = VR(1:nlev)
   IF(INDEX(var, "RLTBLET")>0)    lung(1:nlev)%RLTBLET     = VR(1:nlev)
   IF(INDEX(var, "RLALVPLAS")>0)  lung(1:nlev)%RLalvPlas   = VR(1:nlev)
   IF(INDEX(var, "RLALVLTB")>0)   lung(1:nlev)%RLalvLTB    = VR(1:nlev)
   IF(INDEX(var, "RLALVLINT")>0)  lung(1:nlev)%RLalvLint   = VR(1:nlev)
   IF(INDEX(var, "RLINTPLAS")>0)  lung(1:nlev)%RLintPlas   = VR(1:nlev)
END SUBROUTINE Read_lung_data


SUBROUTINE Read_iter_data()
  IF(var=="MEDIA")         iter%media = VI(1)
  IF(var=="SUBTYPE")     iter%subtype = VI(1)
  IF(var=="DUSTSOIL")   iter%dustsoil = VI(1)
  IF(var=="TARGETBLL") iter%targetbll = VR(1)
  IF(var=="PRECISION") iter%precision = VR(1)
  IF(var=="MAXITER")     iter%maxiter = VI(1)
  IF(var=="AGEWIDTH")   iter%agewidth = VI(1)
  IF(var=="AGEMIN")       iter%agemin = VI(1)
  IF(var=="METRIC")       iter%metric = VI(1)
  IF(var=="GSD")             iter%gsd = VR(1)
  IF(var=="TAILFRAC")   iter%tailfrac = VR(1)
  IF(var=="TAILFRAC")  PRINT*, "iter settings = ", iter
END SUBROUTINE Read_iter_data


SUBROUTINE Findtarget()
   REAL(RK) :: q, x
   CHARACTER(5) :: adjustmedia
   INTEGER :: J
   IF (iter%media<1 .OR. iter%media>6) THEN
       PRINT*, "Adjustable media out of range = ", iter%media; PRINT*
       STOP
   ENDIF 
   print*,  "iter%media    = ", iter%media
   print*,  "iter%subtype  = ", iter%subtype
   DO J=1, Nsource
      print*, j, " Allsource = ", Allsource(J) 
      IF (iter%subtype>0) THEN 
         IF (Allsource(J)%nmedia==iter%media .AND. Allsource(J)%nsub==iter%subtype) adjustable(J)=1 
      ELSE
         IF (Allsource(J)%nmedia==iter%media) adjustable(J)=1 
      ENDIF
      IF (iter%dustsoil==1) THEN
         IF (Allsource(J)%nmedia==2 .OR. Allsource(J)%nmedia==3) adjustable(J)=1 
      ENDIF    
   ENDDO    
   PRINT*, "which=", adjustable
   IF (iter%targetBLL<=0.) THEN
      PRINT*, "Target BLL not positive"; PRINT*
      STOP
   ENDIF
   q = iter%tailfrac
   IF (q<=0. .OR. q>0.5) THEN
      PRINT*, "Tail fraction out of range"; PRINT*
      STOP
   ENDIF
   IF (iter%precision<=0. .OR. iter%precision>0.5) THEN
      PRINT*, "Iterative precision out of range"; PRINT*
      STOP
   ENDIF
   IF (q<0.009) THEN
      x = SQRT(-2D0*LOG(q)-LOG(-6.28318530718*LOG(6.28318530718*q*q)))
   ELSE IF (q<0.125) THEN
      x = -0.7210562 - 1.3675041*LOG(q) - 0.3363243*LOG(q)**2 - 0.07086738*LOG(q)**3             &
          -0.009734347*LOG(q)**4 - 0.0007684821*LOG(q)**5 - 0.00002645659*LOG(q)**6
   ELSE IF (q<0.34) THEN
      x = 2.07894 - 11.6046*q + 50.0513*q**2 - 174.933*q**3 + 388.540*q**4                       &
          -494.168*q**5 + 273.051*q**6
   ELSE
      x = 1.76182 - 6.27925*q + 11.1531*q**2 - 17.0563*q**3 + 14.4313*q**4 - 5.77253*q**5   
   ENDIF
   BLL%center = iter%targetBLL / iter%GSD**x
   BLL%lower  = BLL%center * (1 - iter%precision)
   BLL%upper  = BLL%center * (1 + iter%precision)
   PRINT*, "Iteration target range"
   PRINT '(3F12.5)', BLL%lower, BLL%center, BLL%upper
   PRINT*
END SUBROUTINE Findtarget


SUBROUTINE Iteration_output()
   REAL(RK) :: BLLmetric, runavg
   INTEGER  :: lower, upper, J, div
   BLL%previous = BLL%actual
   IF (iter%agemin>=0) THEN
      iter%agewidth = MIN(sim%agemax-iter%agemin, iter%agewidth)
      lower = ts_age(iter%agemin)
      upper = ts_age(iter%agemin + iter%agewidth)
      PRINT*, "BLL calc 1 =", lower, upper, SUM(Cout(Cblood, lower:upper))
      IF (iter%metric==0) BLL%actual = SUM(Cout(Cblood, lower:upper)) / REAL(upper-lower+1,RK) 
      IF (iter%metric==1) BLL%actual = MAXVAL(Cout(Cblood, lower:upper))
      PRINT*, "BLL res  1 =", BLL%actual
   ELSE
      div = 1+iter%agewidth*Nperday
      BLL%actual = 0.
      PRINT*, "BLL calc 2 =", div
      DO J = 0, NTS-iter%agewidth*Nperday 
         runavg = SUM(Cout(Cblood, J:J+div-1)) / REAL(div,RK)
         IF (BLL%actual < runavg) BLL%actual = runavg
      ENDDO    
      PRINT*, "BLL res  2 =", BLL%actual
   ENDIF   
   PRINT '(A15, I5)',    "Iteration #    ", iteration
   PRINT '(A15, F10.4)', "BLL previous = ", BLL%previous
   PRINT '(A15, F10.4)', "BLL current  = ", BLL%actual
   PRINT '(A15, F10.4)', "BLL target   = ", BLL%center
   PRINT '(A15, F12.6)', "Rel. Error   = ", 1.0 - BLL%actual/BLL%center; PRINT*; PRINT*
   IF (iteration==1 .AND. BLL%actual>BLL%lower) THEN
      PRINT*, "Background sources too high to achieve target"; PRINT*
      WRITE(15,*) "Background sources too high to achieve target" 
      STOP
   ENDIF    
END SUBROUTINE Iteration_output


SUBROUTINE Read_phys_data()
   CHARACTER(6), PARAMETER :: PCList(50) =                                                          &
      (/"ASHWT ", "BLDMOT", "BONIN ", "BRANIN", "BRATIO", "CRTWT ", "H1TOBL", "H1TOH2", "H1TOSI",   &
        "HEPIN ", "IFETAL", "KWT   ", "PLSVOL", "POWER ", "RBCIN ", "RBCNL ", "RBCVOL", "RENIN ",   &
        "RKDN1 ", "RLLI  ", "RLVR1 ", "RPLAS ", "RPROT ", "RSIC  ", "RSOF0 ", "RSOF1 ", "RSOF2 ",   &
        "RSTMC ", "RULI  ", "S2HAIR", "SATRAT", "SIZEVF", "SOFIN ", "TBONEL", "TEVF  ", "TOFECE",   &
        "TOKDN1", "TOKDN2", "TOLVR1", "TOPROT", "TORBC ", "TOSWET", "TOURIN", "TRBWT ", "VBLC  ",   &
        "VKC   ", "VLC   ", "VLUC  ", "HCTA  ", "HCTB  "/)
   CHARACTER(6), PARAMETER :: PVList(22) =                                                          &
       (/"AMTBLD", "F1    ", "FLONG ", "GSCAL ", "RBLAD ", "RBRAN ", "RCORT ", "RCS2B ", "RCS2DF",  &
         "RDIFF ", "RKDN2 ", "RLVR2 ", "RRBC  ", "RTRAB ", "RTS2B ", "RTS2DF", "TBONE ", "TFRAC ",  &
         "TOBRAN", "TOSOF0", "TOSOF1", "TOSOF2" /)
   INTEGER :: varnum, age, first, ts1, ts2, n, offend, phys_age(N1)  
   REAL(RK) :: frac, a, b
   CHARACTER(6) :: var6
   IF (nlev==1) THEN
      var6 = var 
      varnum = FINDLOC(PCList, var6, DIM=1)
      IF(varnum==0) PRINT*, "Phys variable not found: ", var
      IF(varnum>0)  PC(varnum) = VR(1)
   ENDIF
   IF (nlev>1) THEN
      IF(.NOT.ALLOCATED(PVarray)) THEN
         ALLOCATE(PVarray(NUM_PV, 0:NTS), STAT=allocerr) 
         PVarray = 0.
         ALLOCATE(PVrow(0:NTS), STAT=allocerr)
      ENDIF   
      IF (var=="AGE") THEN
         phys_age(1:nlev) = VI(1:nlev)  
      ELSE 
         var6 = var  
         varnum = FINDLOC(PVList, var6, DIM=1)
         IF(varnum==0) PRINT*, "Phys variable not found: ", var
         IF(varnum>0) THEN
            PVin(varnum, 1:nlev) = VR(1:nlev)
            CALL interpolate(nlev, phys_age(1:nlev), VR(1:nlev), timeseries(0:NTS))
            PVarray(varnum, 0:NTS) = timeseries(0:NTS)
            IF(sim%outwrite>0) THEN
               WRITE(12,*) PVList(varnum)
               WRITE(12,'(10E12.4)') PVarray(varnum, indx_write)
            ENDIF
            ! Ensure maximum input value of TBONE is copied to PC(TBoneL)
            IF(var6=="TBone") PC(TBoneL) = VR(nlev)
         ENDIF
      ENDIF   
   ENDIF
END SUBROUTINE Read_phys_data
  

SUBROUTINE Read_media_data()
   INTEGER :: n, I, conc_age(0:N1), intake_age(0:N1), nmedia, nsrc
   INTEGER :: nsuff, age, first, last, base
   REAL(RK) :: div
   CHARACTER(1) :: nchar
   CHARACTER(6) :: name
   nmedia = FINDLOC(med3, vartype, DIM=1) 
   PRINT*, "Media line : ", var, vartype, nmedia, med3(nmedia)
   IF (var=="SOURCES") THEN
      n = VI(1)
      Nbymedia(nmedia) = n
      base = 3*nmedia-3
      DO I=1,n
         WRITE(nchar,'(I1)') I 
         Allsource(base+I)%name   = ADJUSTL(TRIM(media(nmedia))//nchar)
         Allsource(base+I)%media  = media(nmedia) 
         Allsource(base+I)%nmedia = nmedia
         Allsource(base+I)%nsub   = I
         PRINT*, "New Allsource = ", Allsource(base+I)
      ENDDO
   ENDIF 
   IF (var=="CONC_AGES" .OR. INDEX(var,"SOURCE_AGE")>0) THEN
      conc_age(1:nlev) = VI(1:nlev)
      conc_age(0) = nlev 
   ENDIF    
   IF(INDEX(var,"CONCS")>0 .OR. INDEX(var,"SOURCE_AMT")>0) THEN
      n = LEN_TRIM(var) 
      nchar = var(n:n)
      READ(nchar,*) nsrc
      name = TRIM(media(nmedia))//nchar
      IF (Sim%interp==0) CALL stepwise(nlev, conc_age(1:nlev), VR(1:nlev), timeseries(0:NTS)) 
      IF (Sim%interp==1) CALL interpolate(nlev, conc_age(1:nlev), VR(1:nlev), timeseries(0:NTS))
      conc(nsrc,0:NTS) = timeseries(0:NTS)
   ENDIF 
   IF(INDEX(var,"SOURCE_AMT")>0) THEN
      intake_tot  = 1.
      intake_frac = 1.
   ENDIF    
   IF(var=="INTAKE_AGES") THEN
      intake_age(1:nlev) = VI(1:nlev)
      intake_age(0) = nlev
   ENDIF 
   IF(var=="INTAKE_AMT") THEN
      IF (Sim%interp==0) CALL stepwise(nlev, intake_age(1:nlev), VR(1:nlev), timeseries(0:NTS))
      IF (Sim%interp==1) CALL interpolate(nlev, intake_age(1:nlev), VR(1:nlev), timeseries(0:NTS))
      intake_tot(0:NTS) = timeseries(0:NTS) 
   ENDIF
   IF(INDEX(var,"FRAC")>0) THEN
      nchar = var(5:5)
      READ(nchar,*) nsrc
      name = TRIM(media(nmedia))//nchar
      IF(nsrc<Nbymedia(nmedia)) THEN
         IF (Sim%interp==0) CALL stepwise(nlev, intake_age(1:nlev), VR(1:nlev), timeseries(0:NTS))
         IF (Sim%interp==1) CALL interpolate(nlev, intake_age(1:nlev), VR(1:nlev), timeseries(0:NTS))
         intake_frac(nsrc,0:NTS) = timeseries(0:NTS) 
      ELSE
         IF(nsrc==3) intake_frac(3,0:NTS) = 1.- intake_frac(1,0:NTS) - intake_frac(2,0:NTS)
         IF(nsrc==2) intake_frac(2,0:NTS) = 1.- intake_frac(1,0:NTS)
         IF(nsrc==1) intake_frac(1,0:NTS) = 1.
      ENDIF    
   ENDIF
   ! Masks must occur before RBA but after everything else for this media.
   IF(INDEX(var,"MASK")>0) THEN
      nsrc = VI(1)
      ! PRINT*, " Mask line 1 = ", VI(1:4)
      daynum(1:NTS) = sim%agemin + CEILING(age_ts(1:NTS)) + 1 - VI(3) + VI(2)
      ! PRINT*, " Mask line 2 = ", daynum(1:10)
      daynum(1:NTS) = MOD(daynum(1:NTS)-1, VI(2))
      ! print*, " Mask line 3 = ", daynum(1:10)
      mask10(1:NTS) = REAL(daynum(1:NTS),8)/REAL((VI(4)-VI(3)+1),8)
      ! PRINT*, " Mask line 4 = ", mask10(1:10)
      daynum(1:NTS) = MIN(1,INT(mask10(1:NTS)+eps))
      ! PRINT*, " Mask line 5 = ", daynum(1:10)
      mask10(1:NTS) = REAL(daynum(1:NTS),RK)
      ! PRINT*, " Mask line 6 = ", mask10(1:10)
      conc(nsrc, 1:NTS) = conc(nsrc, 1:NTS) * mask10(1:NTS)
   ENDIF
   ! RBA must be the last line for each media. Then do final processing for this media.
   IF(INDEX(var,"RBA")>0) THEN
      first = 3*nmedia - 2
      last  = 3*nmedia
      Allsource(first:last)%RBA = VR(1:nlev)
      div = REAL(Nperday)
      DO J=1,3
         source(first+J-1,1:NTS) = conc(J,1:NTS)*intake_tot(1:NTS)*intake_frac(J,1:NTS)/div
         PRINT*, "source_line1", J, nmedia, first
         PRINT*, "source_line2", SUM(conc(J,1:NTS)), SUM(intake_tot(1:NTS)), SUM(intake_frac(J,1:NTS))
      ENDDO 
   ENDIF    
END SUBROUTINE Read_media_data


SUBROUTINE stepwise(nlev, ages, VR, steps)
   INTEGER, INTENT(IN)  :: nlev, ages(1:nlev) 
   REAL(RK),INTENT(IN)  :: VR(1:nlev)
   REAL(RK),INTENT(OUT) :: steps(0:NTS)
   INTEGER :: J, ts1
   REAL(RK) :: age
   full_ts(:) = VR(1)
   DO J=2, nlev
      ts1 = ages(J)*Nperday+1
      full_ts(ts1:) = VR(J) 
   ENDDO
   steps(0:NTS) = full_ts(sim%agemin*Nperday:sim%agemax*Nperday)
   RETURN
END SUBROUTINE stepwise


SUBROUTINE interpolate(nlev, ages, VR, interp)
   INTEGER, INTENT(IN)  :: nlev, ages(1:nlev) 
   REAL(RK), INTENT(IN)  :: VR(1:nlev)
   REAL(RK),INTENT(OUT) :: interp(0:NTS)
   INTEGER :: ts1, ts2, n, J
   REAL(RK) :: a, b
   ts1 = ages(1)*Nperday+1 
   a   = VR(1)
   full_ts(:) = a
   DO J=2, nlev 
      ts2 = ages(J)*Nperday
      n   = ts2-ts1+1
      b   = VR(J)
      full_ts(ts1:ts2) = (a*REAL(2*n-2*indx_full(1:n)+1)+b*REAL(2*indx_full(1:n)-1))/REAL(2*n)
      full_ts(ts2+1:) = VR(J) 
      ts1 = ts2+1
      a = b
      IF (ages(J)>=sim%agemax) EXIT
   ENDDO
   interp(0:NTS) = full_ts(sim%agemin*Nperday:sim%agemax*Nperday)
END SUBROUTINE interpolate


SUBROUTINE InputSummary()
   PRINT*; PRINT*, " End of reading and interpolating inputs"
   PRINT*, " ********* "; PRINT*;
   WRITE(15,*) "Reading inputs complete"
   WRITE(15,*) "   Age at start    = ", sim%agemin
   WRITE(15,*) "   Age at end      = ", sim%agemax
   WRITE(15,*) "   Number of days  = ", Ndays
   WRITE(15,*) "   Total Timesteps = ", NTS
   WRITE(15,*) "   Type of run     = ", sim%iterate
   WRITE(15,*) 
END SUBROUTINE InputSummary


SUBROUTINE Adjust_inputs()
   INTEGER  :: I, J, first, last, N10
   INTEGER  :: days(0:36500)= (/ (I, I = 0, 36500) /)
   REAL(RK)  :: wgt(0:36500) 
   REAL(RK) :: wgtsum 
   IF (sim%debug==1) THEN
      DO I=1,NSource
         PRINT*, I, Allsource(I)%name
         PRINT*, Source(I,1:NTS:sim%outwrite)
      ENDDO
   ENDIF  
   !PRINT*, "Got here"
   !PRINT*, "gr =  ", gr%wbirth, gr%wchild, gr%half, gr%wadult, gr%kappa, gr%lambda
   !PRINT*, "age_ts range", MINVAL(age_ts(0:NTS)), MAXVAL(age_ts(0:NTS))
   TS(Body_wt, 0:NTS)    = gr%wbirth + age_ts(0:NTS)/365.*gr%wchild/(gr%half+age_ts(0:NTS)/365.) +    &
                           gr%wadult/(1.0 + gr%kappa*EXP(-gr%lambda*gr%wadult*age_ts(0:NTS)/365.))
   !TS(Hematocrit, 0:NTS) = gr%hcta+ (gr%hctb-gr%hcta)*EXP(-13.9*age_ts(0:NTS)/365.)
   TS(Hematocrit, 0:NTS) = PC(HCTA) + (PC(HCTB)-PC(HCTA))*EXP(-13.9*age_ts(0:NTS)/365.)
   TS(Blood_vol,  0:NTS) = PC(VBLC) * 10. * TS(Body_wt, 0:NTS)     ! Factor of 10 to obtain deciliters
   TS(F1_abs,     0:NTS) = PVarray(F1, 0:NTS)
   TS(RBC_vol,    0:NTS) = TS(Blood_vol, 0:NTS) * TS(Hematocrit, 0:NTS)
   TS(Plas_vol,   0:NTS) = TS(Blood_vol, 0:NTS) * (1.-TS(Hematocrit, 0:NTS))
   PRINT*, "Hematocrit, RBC_vol, Plas_vol at birth = ", TS(Hematocrit, 0), TS(RBC_vol, 0), TS(Plas_vol, 0)
   !
   wgtsum                = gr%wbirth + gr%wchild + gr%wadult
   TS(Bone_wt,  0:NTS)   = 1000. * 0.0290 * TS(Body_wt, 0:NTS)**1.21
   TS(Bone_vol, 0:NTS)   = 1000. * 0.0168 * TS(Body_wt, 0:NTS)**1.188
   TS(Cort_wt,  0:NTS)   = TS(Bone_wt, 0:NTS) * 0.80
   TS(Kid_wt,   0:NTS)   = 1050. * PC(VKC) * wgtsum * (TS(Body_wt, 0:NTS)/wgtsum)**0.84
   TS(Liv_wt,   0:NTS)   = 1050. * PC(VLC) * wgtsum * (TS(Body_wt, 0:NTS)/wgtsum)**0.85
   TS(Skel_wt,  0:NTS)   = 1000. * 0.058  * TS(Body_wt, 0:NTS)**1.21
   TS(Trab_wt,  0:NTS)   = TS(Bone_wt, 0:NTS) * 0.20
   !
   TS(R_Blad,     0:NTS) = PVarray(RBlad, 0:NTS)
   TS(R_Bran,     0:NTS) = PVarray(RBran, 0:NTS)
   TS(R_CdifCsur, 0:NTS) = (1. - PVarray(FLong, 0:NTS)) * PVarray(RDiff, 0:NTS)
   TS(R_CdifCvol, 0:NTS) =       PVarray(FLong, 0:NTS)  * PVarray(RDiff, 0:NTS)
   TS(R_Cort,     0:NTS) = PVarray(RCort, 0:NTS)
   TS(R_CsurCdif, 0:NTS) = PVarray(Rcs2df, 0:NTS)
   TS(R_CsurPlas, 0:NTS) = PVarray(Rcs2b, 0:NTS)
   TS(R_Kdn2,     0:NTS) = PVarray(RKdn2, 0:NTS)
   TS(R_Lvr2,     0:NTS) = PVarray(RLvr2, 0:NTS)
   TS(R_RBC,      0:NTS) = PVarray(RRBC, 0:NTS)
   TS(R_TdifTsur, 0:NTS) = (1. - PVarray(FLong, 0:NTS)) * PVarray(RDiff, 0:NTS)
   TS(R_TdifTvol, 0:NTS) =       PVarray(FLong, 0:NTS)  * PVarray(RDiff, 0:NTS)
   TS(R_Trab,     0:NTS) = PVarray(RTrab, 0:NTS)
   TS(R_TsurTdif, 0:NTS) = PVarray(Rts2df, 0:NTS)
   TS(R_TsurPlas, 0:NTS) = PVarray(Rts2b, 0:NTS)
   !
   agescl(0:NTS)     = (1.-PC(TEVF)-PVarray(TBone,0:NTS)) / (1.-PC(TEVF)-PVarray(TBone,NTS))
   TS(T_Bone, 0:NTS) = PVarray(TBone, 0:NTS)
   TS(T_EVF,  0:NTS) = PC(TEVF)
   TS(T_Frac, 0:NTS) = PVArray(TFrac, 0:NTS)
   TS(T_Bran, 0:NTS) = PVarray(ToBran, 0:NTS) * agescl(0:NTS)
   TS(T_SI,   0:NTS) = PC(ToFece)             * agescl(0:NTS)
   TS(T_Kdn1, 0:NTS) = PC(ToKdn1)             * agescl(0:NTS)
   TS(T_Kdn2, 0:NTS) = PC(ToKdn2)             * agescl(0:NTS)
   TS(T_Lvr1, 0:NTS) = PC(ToLvr1)             * agescl(0:NTS)
   TS(T_Prot, 0:NTS) = PC(ToProt)             * agescl(0:NTS)
   TS(T_RBC,  0:NTS) = PC(ToRBC)              * agescl(0:NTS)
   TS(T_Sof0, 0:NTS) = PVarray(ToSof0, 0:NTS) * agescl(0:NTS)
   TS(T_Sof1, 0:NTS) = PVarray(ToSof1, 0:NTS) * agescl(0:NTS)
   TS(T_Sof2, 0:NTS) = PVarray(ToSof2, 0:NTS) * agescl(0:NTS)
   TS(T_Swet, 0:NTS) = PC(ToSwet)             * agescl(0:NTS)
   TS(T_Urin, 0:NTS) = PC(ToUrin)             * agescl(0:NTS)
   !
   TS(T_Sum,  0:NTS) = TS(T_Bone,:) + TS(T_Bran,:) + TS(T_EVF,:)  + TS(T_SI,:)   +  &
                       TS(T_Kdn1,:) + TS(T_Kdn2,:) + TS(T_Lvr1,:) + TS(T_Prot,:) +  &
                       TS(T_RBC,:)  + TS(T_Sof0,:) + TS(T_Sof1,:) + TS(T_Sof2,:) +  &
                       TS(T_Swet,:) + TS(T_Urin,:)
   TS(R_Plas, 0:NTS) = PC(RPlas) * TS(T_Sum, 0:NTS)
   TS(R_EVF,  0:NTS) = TS(T_EVF, 0:NTS) * TS(R_Plas, 0:NTS) / PC(SizeVF)
   DO J=1, NSource
      WRITE(12,*) "Source ", J, AllSource(J)%name
      WRITE(12,'(10E12.4)') Source(J, 1:NTS:sim%outwrite)
   ENDDO    
   DO J=1,Num_TS
      WRITE(12,*) J, "Timeseries"
      WRITE(12,'(10E12.4)') TS(J, 0:NTS:sim%outwrite) 
   ENDDO  
   IF (sim%debug==1) THEN
      DO J=1, NSource
         PRINT*, "Allsource ", J, Allsource(J)%name, Allsource(J)%RBA, SUM(Source(J, :)) 
      ENDDO    
   ENDIF   
   PRINT*, " Inputs adjusted and written to main code arrays"
   PRINT*, " ********* "; PRINT*; 
END SUBROUTINE Adjust_inputs   


SUBROUTINE IntakeUptake()
   ! Check if this the first timestep of the day and reset if so
   IF (MOD(T,Nperday)==1 .OR. Nperday==1) THEN
       Intakes(:) = 0.
       Uptakes(:) = 0.
       Excrete(:) = 0.
   ENDIF 
   Intakes(InairTot) = Intakes(InairTot) + SUM(source(1:3, T)   * adjust(1:3))
   LungDep           = Lung(1:3)%DepFracLET + Lung(1:3)%DepFracLTB + Lung(1:3)%DepFracLalv
   Intakes(InairDep) = Intakes(InairDep) + SUM(source(1:3, T)   * adjust(1:3) * LungDep)
   Intakes(InIngest) = Intakes(InIngest) + SUM(source(4:18, T)  * adjust(4:18))
   DO J=4,Nsource
      SELECT CASE(Allsource(J)%Name(1:4))
        CASE("DUST") 
          Intakes(InDust)   = Intakes(InDust) + source(J, T) * adjust(J)
          Uptakes(UpGIDust) = Uptakes(UpGIDust) + SrcT(F18SIPlas, J)
        CASE("SOIL")  
          Intakes(InSoil)   = Intakes(InSoil) + source(J, T) * adjust(J)
          Uptakes(UpGISoil) = Uptakes(UpGISoil) + SrcT(F18SIPlas, J)
        CASE("WATE")
          Intakes(InWater)  = Intakes(InWater) + source(J, T) * adjust(J) 
          Uptakes(UpGIWater)= Uptakes(UpGIWater) + SrcT(F18SIPlas, J)
        CASE("FOOD")
          Intakes(InFood)   = Intakes(InFood) + source(J, T) * adjust(J)  
          Uptakes(UpGIFood) = Uptakes(UpGIFood) + SrcT(F18SIPlas, J)
        CASE("OTHE")
          Intakes(InOther)  = Intakes(InOther) + source(J, T) * adjust(J)
          Uptakes(UpGIOther)= Uptakes(UpGIOther) + SrcT(F18SIPlas, J)
      END SELECT    
   ENDDO    
   Uptakes(UpLung)   = Uptakes(UpLung) + FL(LET_Plas,T) + FL(LTB_Plas,T) + FL(Lalv_Plas,T) + FL(Lint_Plas,T)
   Uptakes(UpGIAir)  = Uptakes(UpGIAir)  + SUM(SrcT(F18SIPlas, 1:3))
   Uptakes(UpAir)    = Uptakes(UpLung) + Uptakes(UpGIAir) 
   Uptakes(UpIngest) = Uptakes(UpIngest) + SUM(SrcT(F18SIPlas, 4:18))
   Uptakes(UpGITotal)= Uptakes(UpGIAir) + Uptakes(UpIngest)
   Uptakes(UpTotal)  = Uptakes(UpAir) + Uptakes(UpIngest)
   Excrete(ExAir)    = Excrete(ExAir)    + Intakes(InairTot) - Intakes(InairDep)
   Excrete(ExUrine)  = Excrete(ExUrine)  + FL(Blad_Urin, T)
   Excrete(ExFeces)  = Excrete(ExFeces)  + FL(LLI_Fece, T)
   Excrete(ExSweat)  = Excrete(ExSweat)  + FL(Plas_Swet, T)
   Excrete(ExHair)   = Excrete(ExHair)   + FL(Sof1_Hair, T)
   IF (T==1) THEN
       IF(sim%iterate==1) REWIND(16)
       WRITE(16,*) "Day,InAirToT,InAirDep,InIngest,InDust,InSoil,InWater,InFood,InOther,UpTotal,"//    &
          "UpAir,UpLung,UpGIAir,UpGITotal,UpIngest,UpGIDust,UpGISoil,UpGIWater,UpGIFood,UpGIOther,"//  &
          "ExAir,ExUrine,ExFeces,ExSweat,ExHair"
   ENDIF    
   IF (MOD(T,Nperday)==0) THEN
       WRITE(16, '(I5,",",23(F18.6,","),F18.6)') T/NPerday, Intakes(1:8), Uptakes(1:11), Excrete(1:5)
   ENDIF 
END SUBROUTINE IntakeUptake


REAL(RK) FUNCTION Actvty(Y, P, X, D)
   REAL(RK), INTENT(IN) :: Y, P, X, D
   IF (X==0.) THEN
      Actvty = Y + P*D
   ELSE
      Actvty = (Y-P/X)*DEXP(-DBLE(X*D)) + P/X
   ENDIF
   RETURN
END FUNCTION Actvty
  
SUBROUTINE ReportRates()
END SUBROUTINE ReportRates

END PROGRAM