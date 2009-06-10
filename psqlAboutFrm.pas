{$I psqldac.inc}
unit psqlAboutFrm;

{SVN revision: $Id$}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons;

type          
  TPSQLAboutComp = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    VersionLabel: TLabel;
    Bevel1: TBevel;
    Label5: TLabel;
    Image1: TImage;
    RegLabel: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
  private
    { Private declarations }
    FVersion : string;
    FCompName : String;
    FRegister : String;
  public
    { Public declarations }
    property Version: string   read FVersion   write FVersion;
    property CompName :string read FCompName write FCompname;
    property RegVersion :string read FRegister write FRegister;
  end;

var
  PSQLAboutComp: TPSQLAboutComp;

procedure Dac4PSQLShowAbout(aComponentName : string);

implementation
uses ShellAPI, PSQLDbTables;

{$R *.DFM}


procedure Dac4PSQLShowAbout(aComponentName : string);
begin
  with TPSQLAboutComp.Create(Application) do
  try
    Caption := 'Thank you for trying PostgresDAC';
    VersionLabel.Caption := 'v.' + PSQLDBTables.VERSION;
    Label1.Caption := aComponentName;

    {$IFDEF MICROOLAP_BUSINESS_LICENSE}
    RegLabel.Caption := 'Business License.';
    {$ELSE}
      {$IFDEF MICROOLAP_COMMERCIAL_LICENSE}
      RegLabel.Caption := 'Commercial License.';
      {$ELSE}
        {$IFDEF MICROOLAP_EDU_CLASSROOM_LICENSE}
        RegLabel.Caption := 'Educational classroom License.';
        {$ELSE}
          {$IFDEF MICROOLAP_EDU_INSTITUTION_LICENSE}
          RegLabel.Caption := 'Educational institution License.';
          {$ELSE}
            {$IFDEF MICROOLAP_PERSONAL_LICENSE}
            RegLabel.Caption := 'Personal License.';
            {$ELSE}
              {$IFDEF TRIAL}
              RegLabel.Caption := 'Trial License.';
              {$ELSE}
              RegLabel.Caption := 'Edited license string => Trial license';
              {$ENDIF}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}

    ShowModal();
  finally
    Free();
  end;
end;

procedure TPSQLAboutComp.FormCreate(Sender: TObject);
begin
  FVersion := '';
  FCompName := '';
  FRegister:='';
  Label2.Caption := FormatDateTime('"(c) 1999-"yyyy" microOLAP Technologies LTD"', Now());
end;

procedure TPSQLAboutComp.SpeedButton1Click(Sender: TObject);
begin
  {Send e-mail}
	ShellExecute(0,'Open','http://www.microolap.com/support/ticket_edit.php',nil,nil,SW_SHOW);
end;

procedure TPSQLAboutComp.SpeedButton2Click(Sender: TObject);
begin
   {Go to web}
   ShellExecute(0,'Open','http://microolap.com/products/connectivity/postgresdac/',nil,nil,SW_SHOW);
end;

procedure TPSQLAboutComp.SpeedButton3Click(Sender: TObject);
begin
   ShellExecute(0,'Open','http://microolap.com/products/connectivity/postgresdac/order/',nil,nil,SW_SHOW);
end;

end.
