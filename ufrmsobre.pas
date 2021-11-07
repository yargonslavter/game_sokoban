unit ufrmsobre;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFrmSobre }

  TFrmSobre = class(TForm)
    imgLogo: TImage;
    lblLink: TLabel;
    lblSite: TLabel;
    lblDescricao: TLabel;
    spL: TShape;
    spT: TShape;
    spR: TShape;
    spB: TShape;
    procedure FormClick(Sender: TObject);
    procedure lblLinkClick(Sender: TObject);
  private

  public

  end;

var
  FrmSobre: TFrmSobre;

implementation

{$R *.lfm}

uses LCLIntf;

{ TFrmSobre }

procedure TFrmSobre.FormClick(Sender: TObject);
begin
  // Clicou, fechou! :D
  Close();
end;

procedure TFrmSobre.lblLinkClick(Sender: TObject);
begin
  // Abrir o site
  OpenURL(lblLink.Caption);
end;


end.

