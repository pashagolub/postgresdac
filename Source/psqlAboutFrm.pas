{$I pSQLDAC.inc}
unit psqlAboutFrm;

{SVN revision: $Id$}

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ENDIF}Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
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

{$IFNDEF FPC}
  {$R *.DFM}
{$ENDIF}

uses ShellAPI, PSQLDbTables;

procedure Dac4PSQLShowAbout(aComponentName : string);
begin
  with TPSQLAboutComp.Create(Application) do
  try
    Caption := 'Thank you for trying PostgresDAC';
    VersionLabel.Caption := 'v.' + PSQLDBTables.VERSION;
    Label1.Caption := aComponentName;
    RegLabel.Caption := 'PostgreSQL License';
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
  Label2.Caption := '(c) Pavlo Golub';
end;

procedure TPSQLAboutComp.SpeedButton1Click(Sender: TObject);
begin
  {Send e-mail}
	ShellExecute(0,'Open','https://github.com/pashagolub/postgresdac/issues',nil,nil,SW_SHOW);
end;

procedure TPSQLAboutComp.SpeedButton2Click(Sender: TObject);
begin
   {Go to web}
   ShellExecute(0,'Open','https://github.com/pashagolub/postgresdac/',nil,nil,SW_SHOW);
end;

procedure TPSQLAboutComp.SpeedButton3Click(Sender: TObject);
begin
   ShellExecute(0,'Open','https://github.com/sponsors/pashagolub/',nil,nil,SW_SHOW);
end;

initialization
{$IFDEF FPC}
  {$i psqlAboutFrm.lrs}
{$ENDIF}

end.
