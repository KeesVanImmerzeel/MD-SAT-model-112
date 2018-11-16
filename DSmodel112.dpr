library dsmodel112;
  {-Bereken welk deel (%) van het knooppunts-invloed-oppervlak de neerslaglens
    (gemiddeld op jaarbasis) dunner is dan een gespecificeerde dikte Dcrit (m).
    Methode volgens Kees Maas. }

  { Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  ShareMem,
  windows, SysUtils, Classes, LargeArrays,
  ExtParU, USpeedProc, uDCfunc, UdsModel, UdsModelS, xyTable, DUtils, uError,
  Math;

Const
  cModelID      = 112;  {-Uniek modelnummer}

  {-Beschrijving van de array met afhankelijke variabelen}
  cNrOfDepVar   = 1;   {-Lengte van de array met afhankelijke variabelen}
  cy1           = 1;   {-Betekenis van afhankelijke variabele 1: Percentage (cumulatief)}

  {-Aantal keren dat een discontinuiteitsfunctie wordt aangeroepen in de procedure met
    snelheidsvergelijkingen (DerivsProc)}
  nDC = 0;

  {-Variabelen die samenhangen met het aanroepen van het model vanuit de Shell}
  cnRP    = 5;   {-Aantal RP-tijdreeksen die door de Shell moeten worden aangeleverd (in
                   de externe parameter Array EP (element EP[ indx-1 ]))}
  cnSQ    = 0;   {-Idem punt-tijdreeksen}
  cnRQ    = 0;   {-Idem lijn-tijdreeksen}

  {-Beschrijving van het eerste element van de externe parameter-array (EP[cEP0])}
  cNrXIndepTblsInEP0 = 3;  {-Aantal XIndep-tables in EP[cEP0]}
  cNrXdepTblsInEP0   = 0;  {-Aantal Xdep-tables   in EP[cEP0]}
  {-Nummering van de xIndep-tabellen in EP[cEP0]. De nummers 0&1 zijn gereserveerd}
  cTb_MinMaxValKeys   = 2;

  {-Beschrijving van het tweede element van de externe parameter-array (EP[cEP1])}
  {-Opmerking: table 0 van de xIndep-tabellen is gereserveerd}
  {-Nummering van de xdep-tabellen in EP[cEP1]}
  cTb_Neerslagoverschot = 0; {-Nuttige neerslag (m/d). Overweeg evt. deze te
                               verminderen met de greppel-afvoer}
  cTb_QRCH              = 1; {-De voeding van het watervoerend pakket (m/d). Negatief = kwel}
  cTb_Aniso             = 2; {-Anisotropie ky/dx in het topsysteem (-). ky/kx > 1}
  cTb_Slootafstand      = 3; {-Slootafstand (m)}
  cTb_Dcrit             = 4; {-Kritische dikte van de neerslaglens (m)}

  {-Model specifieke fout-codes}
  cInvld_Neerslagoverschot = -9830;
  cInvld_QRCH              = -9831;
  cInvld_Aniso             = -9832;
  cInvld_Slootafstand      = -9833;
  cInvld_Dcrit             = -9834;

var
  Indx: Integer; {-Door de Boot-procedure moet de waarde van deze index worden ingevuld, 
                   zodat de snelheidsprocedure 'weet' waar (op de externe parameter-array) 
                   hij zijn gegevens moet zoeken}
  ModelProfile: TModelProfile;
                 {-Object met met daarin de status van de discontinuiteitsfuncties (zie nDC) }
				   
  {-Geldige range van key-/parameter/initiele waarden. De waarden van deze  variabelen moeten
    worden ingevuld door de Boot-procedure}
  cMin_Neerslagoverschot, cMax_Neerslagoverschot,
  cMin_QRCH, cMax_QRCH,
  cMin_Aniso, cMax_Aniso,
  cMin_Slootafstand, cMax_Slootafstand,
  cMin_Dcrit, cMax_Dcrit: Double;

Procedure MyDllProc( Reason: Integer );
begin
  if Reason = DLL_PROCESS_DETACH then begin {-DLL is unloading}
    {-Cleanup code here}
	if ( nDC > 0 ) then
      ModelProfile.Free;
  end;
end;

Procedure DerivsProc( var x: Double; var y, dydx: TLargeRealArray;
                      var EP: TExtParArray; var Direction: TDirection;
                      var Context: Tcontext; var aModelProfile: PModelProfile; var IErr: Integer );
{-Deze procedure verschaft de array met afgeleiden 'dydx', gegeven het tijdstip 'x' en
  de toestand die beschreven wordt door de array 'y' en de externe condities die beschreven 
  worden door de 'external parameter-array EP'. Als er geen fout op is getreden bij de 
  berekening van 'dydx' dan wordt in deze procedure de variabele 'IErr' gelijk gemaakt aan de 
  constante 'cNoError'. Opmerking: in de array 'y' staan dus de afhankelijke variabelen, 
  terwijl 'x' de onafhankelijke variabele is (meestal de tijd)}
var
  Neerslagoverschot, QRCH, Aniso,      {-Parameter-waarden afkomstig van de Shell}
  Slootafstand, Dcrit,
  Percentage: Double;                  {-Afgeleide (berekende) parameter-waarden}
  i: Integer;

Function SetParValuesFromEP0( var IErr: Integer ): Boolean;
  {-Fill globally defined parameters from EP[0]. If memory is allocated here,
    free first with 'try .. except' for those cases that the model is used repeatedly}
begin
  Result := true;
end;

Function SetKeyAndParValues( var IErr: Integer ): Boolean;
  
  Function GetParFromShell_Neerslagoverschot( const x: Double ): Double;
  begin  
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_Neerslagoverschot ].EstimateY( x, Direction );
  end;
  Function GetParFromShell_QRCH( const x: Double ): Double;
  begin  
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_QRCH ].EstimateY( x, Direction );
  end;
  Function GetParFromShell_Aniso( const x: Double ): Double;
  begin  
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_Aniso ].EstimateY( x, Direction );
  end;
  Function GetParFromShell_Slootafstand( const x: Double ): Double;
  begin  
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_Slootafstand ].EstimateY( x, Direction );
  end;
  Function GetParFromShell_Dcrit( const x: Double ): Double;
  begin  
    with EP[ indx-1 ].xDep do
      Result := Items[ cTb_Dcrit ].EstimateY( x, Direction );
  end;

  Function DMax( const SltAfst, kykx, RP1, QRCH: Double ): Double;
  begin
    Result := Max( SltAfst * Sqrt( kykx ) * ArcTanh( RP1 / ( RP1 -QRCH ) ) / pi, 0 );
  end;

  Function GetPercentage( const Dcrit, DMax: Double ): Double;
  begin
    if ( Dcrit > Dmax ) or ( DMax = 0 ) then begin {-Dmax >=0 dus ook Dcrit >=0 }
      GetPercentage := 100.0
    end else begin
      if ( Dcrit <= 0 ) then
        GetPercentage := 0.0
      else
        GetPercentage := 100 * ( 1 - Sqrt( ( 1 - Dcrit / Dmax ) ) );
    end;
  end;
  
begin {-Function SetKeyAndParValues}
  Result := False;

  Neerslagoverschot := GetParFromShell_Neerslagoverschot( x );
  if ( Neerslagoverschot < cMin_Neerslagoverschot ) or ( Neerslagoverschot > cMax_Neerslagoverschot ) then begin
    IErr := cInvld_Neerslagoverschot; Exit;
  end;

  QRCH := GetParFromShell_QRCH( x );
  if ( QRCH < cMin_QRCH ) or ( QRCH > cMax_QRCH ) then begin
    IErr := cInvld_QRCH; Exit;
  end;

  Aniso := GetParFromShell_Aniso( x );
  if ( Aniso < cMin_Aniso ) or ( Aniso > cMax_Aniso ) then begin
    IErr := cInvld_Aniso; Exit;
  end;

  Slootafstand := GetParFromShell_Slootafstand( x );
  if ( Slootafstand < cMin_Slootafstand ) or ( Slootafstand > cMax_Slootafstand ) then begin
    IErr := cInvld_Slootafstand; Exit;
  end;

  Dcrit := GetParFromShell_Dcrit( x );
  if ( Dcrit < cMin_Dcrit ) or ( Dcrit > cMax_Dcrit ) then begin
    IErr := cInvld_Dcrit; Exit;
  end;

  Percentage := GetPercentage( Dcrit, DMax( SlootAfstand, Aniso,
                Neerslagoverschot, QRCH ) );

  Result := True; IErr := cNoError;
end; {-Function SetKeyAndParValues}

Function Replace_InitialValues_With_ShellValues( var IErr: Integer): Boolean;
  {-Als de Shell 1-of meer initiele waarden aanlevert voor de array met afhankelijke
    variabelen ('y'), dan kunnen deze waarden hier op deze array worden geplaatst en
    gecontroleerd}
begin
    IErr := cNoError; Result := True;
//  with EP[ indx-1 ].xDep do
//    y[ ### ] := Items[ cTB_### ].EstimateY( 0, Direction ); {Opm.: x=0}
//  if ( y[ ### ] < cMin_InitVal1 ) or
//     ( y[ ### ] > cMax_InitVal1 ) then begin
//    IErr := cInvld_Init_Val1; Result := False; Exit;
//  end;
end; {-Replace_InitialValues_With_ShellValues}

begin {-Procedure DerivsProc}

  IErr := cUnknownError;
  for i := 1 to cNrOfDepVar do {-Default speed = 0}
    dydx[ i ] := 0;

  {-Geef de aanroepende procedure een handvat naar het ModelProfiel}
  if ( nDC > 0 ) then
    aModelProfile := @ModelProfile
  else
    aModelProfile := NIL;

  if not SetKeyAndParValues( IErr ) then 
    exit;                  
  
  if ( Context = UpdateYstart ) then begin {-Run fase 1}

    {-Fill globally defined parameters from EP[0]}
    if not SetParValuesFromEP0( IErr ) then Exit;
	
    {-Optioneel: initiele waarden vervangen door Shell-waarden}
    if not Replace_InitialValues_With_ShellValues( IErr ) then
	  Exit;
	
    {-Bij Shell-gebruik van het model (indx = cBoot2) dan kan het wenselijk zijn de tijd-as
	  van alle Shell-gegevens te converteren, bijvoorbeeld naar jaren}
      {### if ( indx = cBoot2 ) then
        ScaleTimesFromShell( cFromDayToYear, EP ); ### }
    IErr := cNoError;

  end else begin {-Run fase 2}            

    dydx[ cy1 ] := Percentage;  {-Bereken de array met afgeleiden 'dydx'}

  end;
end; {-DerivsProc}

Function DefaultBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Initialiseer de meest elementaire gegevens van het model. Shell-gegevens worden door deze
    procedure NIET verwerkt}
Procedure SetMinMaxKeyAndParValues;
begin
  with EP[ cEP0 ].xInDep.Items[ cTb_MinMaxValKeys ] do begin
    cMin_Neerslagoverschot := GetValue( 1, 1 ); {rij, kolom}
    cMax_Neerslagoverschot := GetValue( 1, 2 );
    cMin_QRCH :=              GetValue( 1, 3 );
    cMax_QRCH :=              GetValue( 1, 4 );
    cMin_Aniso :=             GetValue( 1, 5 );
    cMax_Aniso :=             GetValue( 1, 6 );
    cMin_Slootafstand :=      GetValue( 1, 7 );
    cMax_Slootafstand :=      GetValue( 1, 8 );
    cMin_Dcrit :=             GetValue( 1, 9 );
    cMax_Dcrit :=             GetValue( 1, 10 );
  end;
end;

Begin
  Result := DefaultBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cNrOfDepVar, nDC, cNrXIndepTblsInEP0, 
                                       cNrXdepTblsInEP0, Indx, EP );
  if ( Result = cNoError ) then begin
    SetMinMaxKeyAndParValues;
    SetAnalytic_DerivsProc( True, EP ); {-Ref. 'USpeedProc.pas'}
  end;
end;

Function TestBootEP( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Deze boot-procedure verwerkt alle basisgegevens van het model en leest de Shell-gegevens
    uit een bestand. Na initialisatie met deze boot-procedure is het model dus gereed om
	'te draaien'. Deze procedure kan dus worden gebruikt om het model 'los' van de Shell te 
	testen}
Begin
  Result := DefaultBootEP( EpDir, BootEpArrayOption, EP );
  if ( Result <> cNoError ) then 
    exit;
  Result := DefaultTestBootEPFromTextFile( EpDir, BootEpArrayOption, cModelID, cnRP + cnSQ + cnRQ, Indx, EP );
  if ( Result <> cNoError ) then 
    exit;
  SetReadyToRun( EP);
end;

Function BootEPForShell( const EpDir: String; const BootEpArrayOption: TBootEpArrayOption; var EP: TExtParArray ): Integer;
  {-Deze procedure maakt het model gereed voor Shell-gebruik. 
    De xDep-tables in EP[ indx-1 ] worden door deze procedure NIET geinitialiseerd omdat deze
	gegevens door de Shell worden verschaft }
begin
  Result := DefaultBootEP( EpDir, cBootEPFromTextFile, EP );
  if ( Result = cNoError ) then
    Result := DefaultBootEPForShell( cnRP, cnSQ, cnRQ, Indx, EP );
end;

Exports DerivsProc       index cModelIndxForTDSmodels, {999}
        DefaultBootEP    index cBoot0, {1}
        TestBootEP       index cBoot1, {2}
        BootEPForShell   index cBoot2; {3}
		
begin
  {-Dit zgn. 'DLL-Main-block' wordt uitgevoerd als de DLL voor het eerst in het geheugen wordt
    gezet (Reason = DLL_PROCESS_ATTACH)}
  DLLProc := @MyDllProc;
  Indx := cBootEPArrayVariantIndexUnknown;
  if ( nDC > 0 ) then
    ModelProfile := TModelProfile.Create( nDC );
end.
