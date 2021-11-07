unit ufrmmenu;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Controls, Dialogs, StdCtrls, ExtCtrls, UFrmMain,
  ufrmescolhermapa, ufrmsobre;

type

  { TFrmMenu }

  TFrmMenu = class(TForm)
    btnContinuar: TButton;
    btnIniciar: TButton;
    btnConfiguracoes: TButton;
    btnSobre: TButton;
    btnSair: TButton;
    imgFundo: TImage;
    lblTitulo: TLabel;
    spLeft: TShape;
    spRight: TShape;
    spTop: TShape;
    spBottom: TShape;
    procedure btnConfiguracoesClick(Sender: TObject);
    procedure btnIniciarClick(Sender: TObject);
    procedure btnSairClick(Sender: TObject);
    procedure btnSobreClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FrmMenu: TFrmMenu;

implementation

{$R *.lfm}

uses usokoban, ufrmrecursos;

{ TFrmMenu }

procedure TFrmMenu.btnSobreClick(Sender: TObject);
begin
  // Sobre
  FrmSobre := TFrmSobre.Create(Self);
  try
    FrmSobre.ShowModal();
  finally
    FreeAndNil(FrmSobre);
  end;
end;

procedure TFrmMenu.FormCreate(Sender: TObject);
begin
  // OnCreate
  FrmRecursos := TFrmRecursos.Create(Self);
  FrmRecursos.CarregarSprites();

  // Sokoban
  Sokoban := TLZSokoban.Create();
  Sokoban.CarregarMapas();

  // Imagem de fundo
  imgFundo.Picture.Assign(FrmRecursos.imgFundo.Picture);
end;

procedure TFrmMenu.btnIniciarClick(Sender: TObject);
begin
  // Iniciar jogo
  FrmEscolherMapa := TFrmEscolherMapa.Create(Self);
  try
    FrmEscolherMapa.ShowModal();
    Self.BringToFront();
  finally
    FreeAndNil(FrmEscolherMapa);
  end;
  {
  if Sokoban.Mapas_Tutoriais.Total = 0 then
    Exit;

  FrmMain := TFrmMain.Create(Application);
  try
    Self.Hide();
    FrmMain.Jogar(Sokoban.Mapas_Tutoriais.Obter(0));
  finally
    Self.Show();
    Self.BringToFront();
    FreeAndNil(FrmMain);
  end;
  }
end;

procedure TFrmMenu.btnSairClick(Sender: TObject);
begin
  // Sair
  Self.Close();
end;

procedure TFrmMenu.btnConfiguracoesClick(Sender: TObject);
begin
  // Configurações

end;

end.

