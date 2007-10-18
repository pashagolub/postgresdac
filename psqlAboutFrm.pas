unit psqlAboutFrm;

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

implementation
uses ShellAPI;

{$R *.DFM}

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
