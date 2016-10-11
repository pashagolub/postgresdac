unit fmMobileGrid;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.Grid.Style, Data.Bind.EngExt, Fmx.Bind.DBEngExt, Fmx.Bind.Grid,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.Components,
  Data.Bind.Grid, Data.Bind.DBScope, FMX.ScrollBox, FMX.Grid,
  FMX.Controls.Presentation, FMX.StdCtrls, Data.DB, PSQLDbTables, FMX.Memo;

type
  TForm1 = class(TForm)
    PSQLDatabase1: TPSQLDatabase;
    Connect: TButton;
    Label1: TLabel;
    PSQLQuery1: TPSQLQuery;
    Memo1: TMemo;
    procedure ConnectClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.SmXhdpiPh.fmx ANDROID}


procedure TForm1.ConnectClick(Sender: TObject);
begin
  PSQLDatabase1.Open;
  Label1.Text := PSQLDatabase1.ServerVersion;
  PSQLQuery1.Open;
  while not PSQLQuery1.Eof do
    Memo1.Lines.Append(PSQLQuery1.Fields[0].AsString);
end;

end.
