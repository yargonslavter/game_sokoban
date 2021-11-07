unit UFrmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Menus, Grids, ComCtrls, zipper, usokoban, ufrmrecursos, Types;

type
  { TFrmMain }

  TFrmMain = class(TForm)
    dgMapa: TDrawGrid;
    imgFundo: TImage;
    imgFechar: TImage;
    lblTitulo: TLabel;
    lblPassos: TLabel;
    lblTempo: TLabel;
    pnlCentro: TPanel;
    pnlBaixo: TPanel;
    pnlTopo: TPanel;
    timerMain: TTimer;
    procedure dgMapaDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure imgFecharClick(Sender: TObject);
    procedure timerMainTimer(Sender: TObject);
  private
    FDebug: Boolean;
    FEscala: Integer;
    FTempo: Double;
    FMapa: TMapa;
    FZip: TZipper;
    procedure SetEscala(AValue: Integer);
  public
    procedure AjustaTela();
    procedure CentralizarMapa();
    function Jogar(const mapa: TMapa): Boolean;
  published
    property Debug: Boolean read FDebug write FDebug;
    property Escala: Integer read FEscala write SetEscala;
    property Mapa: TMapa read FMapa;
  end;


var
  FrmMain: TFrmMain;

implementation

{$R *.lfm}

uses LazFileUtils, fpjson, LCLType;

{ TFrmMain }

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  // On Create
  Color := dgMapa.Color;
  FMapa := TMapa.Create();
  FZip := TZipper.Create();

  // Fundo
  imgFundo.Picture.Assign(FrmRecursos.imgFundo.Picture);

  //
  FTempo := 0;
  FEscala := 2;
end;

procedure TFrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  atualizou: Boolean;
begin
  // OnKeyDown do Form
  if not Assigned(Mapa) or not Assigned(Mapa.Personagem) then
    Exit;

  //
  atualizou := False;
  case Key of
    VK_UP,
    VK_W:
    begin
      atualizou := Mapa.Personagem.Mover(pdCima);
    end;
    VK_DOWN,
    VK_S:
    begin
      atualizou := Mapa.Personagem.Mover(pdBaixo);
    end;
    VK_LEFT,
    VK_A:
    begin
      atualizou := Mapa.Personagem.Mover(pdEsquerda);
    end;
    VK_RIGHT,
    VK_D:
    begin
      atualizou := Mapa.Personagem.Mover(pdDireita);
    end;

    VK_R:
    begin
      Self.Mapa.Resetar();
      atualizou := True;
    end;
    // DEBUG KEYs
    VK_F11:
    begin
      FrmRecursos.Show();
    end;
    VK_F12:
    begin
      Debug := not Debug;
      atualizou := True;
    end;
  end;

  // Aplicar atualização
  if atualizou then
  begin
    dgMapa.Invalidate();
    CentralizarMapa();
  end;
end;

procedure TFrmMain.imgFecharClick(Sender: TObject);
begin
  // Botão fechar
  Close();
end;

procedure TFrmMain.dgMapaDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  s: string;
  obj: TObjetivo = nil;
  cx: TCaixa = nil;
begin
  if not Assigned(Mapa) then
    Exit;

  s := Mapa.PegarTile(aCol, aRow);

  // Desenhar tile
  if (s = CO_JSON_VAZIO) then
    dgMapa.Canvas.StretchDraw(aRect, FrmRecursos.imgChao.Picture.Graphic)
  else if (s = CO_JSON_PAREDE) then
    dgMapa.Canvas.StretchDraw(aRect, FrmRecursos.imgParede.Picture.Graphic)
  else
  begin
    // Não deveria chegar aqui
    dgMapa.Canvas.Brush.Color := clFuchsia;
    dgMapa.Canvas.FillRect(aRect);
  end;

  // Desenhar objetivo
  obj := Mapa.Objetivo(aCol, aRow);
  if Assigned(obj) then
  begin
    dgMapa.Canvas.StretchDraw(aRect, FrmRecursos.imgObjetivo.Picture.Graphic);
  end;

  // Desenhar caixa
  cx := Mapa.Caixa(aCol, aRow);
  if Assigned(cx) then
  begin
    if not cx.ObjetivoAbaixo() then
      dgMapa.Canvas.StretchDraw(aRect, FrmRecursos.imgCaixa1.Picture.Graphic)
    else
      dgMapa.Canvas.StretchDraw(aRect, FrmRecursos.imgCaixa2.Picture.Graphic);
  end;

  // Desenhar Personagem
  if Assigned(Mapa.Personagem) and (aCol = Mapa.Personagem.X) and (aRow = Mapa.Personagem.Y) then
  begin
    case Mapa.Personagem.Direcao of
      pdCima: dgMapa.Canvas.StretchDraw(aRect, FrmRecursos.imgPersonagemTop.Picture.Graphic);
      pdBaixo: dgMapa.Canvas.StretchDraw(aRect, FrmRecursos.imgPersonagemBottom.Picture.Graphic);
      pdEsquerda: dgMapa.Canvas.StretchDraw(aRect, FrmRecursos.imgPersonagemLeft.Picture.Graphic);
      pdDireita: dgMapa.Canvas.StretchDraw(aRect, FrmRecursos.imgPersonagemRight.Picture.Graphic);
    end;
  end;


  // Textos de DEBUG
  if Debug then
  begin
    // Indice
    dgMapa.Canvas.Font.Size := 8;
    dgMapa.Canvas.TextOut(aRect.Left, aRect.Top, Format('(%d, %d)', [aCol, aRow]));

    // Valor da matriz
    dgMapa.Canvas.Font.Size := 12;
    dgMapa.Canvas.TextOut(aRect.Left, aRect.Top + 18, Format('%s', [s]));

    // Objetivo
    if Assigned(obj) then
    begin
      dgMapa.Canvas.Font.Size := 8;
      dgMapa.Canvas.Font.Color := clGreen;
      dgMapa.Canvas.TextOut(aRect.Left, aRect.Bottom - 16, Format('%s', [obj.Nome]));
    end;

    // Caixa
    if Assigned(cx) then
    begin
      dgMapa.Canvas.Font.Size := 8;
      dgMapa.Canvas.Font.Color := clRed;
      dgMapa.Canvas.TextOut(aRect.Left, aRect.Bottom - 16, Format('%s', [cx.Nome]));
    end;
  end;
end;

procedure TFrmMain.timerMainTimer(Sender: TObject);
begin
  // Timer
  FTempo := FTempo + (Double(timerMain.Interval) / 1000.0);

  // Atualizar labels
  lblPassos.Caption := 'Passos: ' + IntToStr(Mapa.Personagem.Movimentos) + ', ' +
                       'Movimentos: ' + IntToStr(Mapa.TotalMovimentoCaixas());
  lblTempo.Caption := 'Tempo: ' + FormatFloat('0.0', Self.FTempo);

  // Verificar vitória
  if Mapa.Vitoria() then
  begin
    timerMain.Enabled := False;

    // Verificar se foi recorde
    if Self.Mapa.Recorde.TentarQuebrar(Now(), Self.FTempo, Mapa.Personagem.Movimentos, Mapa.TotalMovimentoCaixas()) then
    begin
      ShowMessage('NOVO RECORDE!');
    end
    else
    begin
      ShowMessage('Nível concluído!');
    end;

    ModalResult := mrOK;
  end;
end;

procedure TFrmMain.SetEscala(AValue: Integer);
begin
  if (FEscala = AValue) or (AValue < 0) then
    Exit;
  FEscala := AValue;

  dgMapa.DefaultColWidth := 32 * FEscala;
  dgMapa.DefaultRowHeight := 32 * FEscala;
  dgMapa.Update();
  AjustaTela();
end;

procedure TFrmMain.AjustaTela();
//var
//   n: Integer;
begin
  // Largura
  dgMapa.Width := dgMapa.ColCount * dgMapa.DefaultColWidth;
  dgMapa.Height := dgMapa.RowCount * dgMapa.DefaultRowHeight;

  {
  ClientWidth := dgMapa.Width;
  ClientHeight := dgMapa.Height + pnlTopo.Height;

  Left := (Screen.Width div 2) - (Width div 2);
  Top := (Screen.Height div 2) - (Height div 2);
  }

  {
  n := Mapa.Largura * CO_LARGURA_TILE * Escala;

  if n > 0 then
    Self.ClientWidth := n
  else
    Self.ClientWidth := 128;

  // Altura
  n := Mapa.Altura * CO_ALTURA_TILE * Escala;

  if n > 0 then
    Self.ClientHeight := n
  else
    Self.ClientHeight := 128;
  }

  Self.UpdateActions();
  Self.Update();
  Self.Refresh();
end;

procedure TFrmMain.CentralizarMapa();
var
   x, y: Integer;
begin
  // Centraliza o mapa na tela (pelo jogador)
  {
  x := (Mapa.Personagem.X * dgMapa.DefaultColWidth);
  y := Mapa.Personagem.Y * dgMapa.DefaultRowHeight;
  }
  x := 0;
  y := 0;

  dgMapa.Left := (pnlCentro.Width div 2) - (dgMapa.Width div 2) - x;
  dgMapa.Top := (pnlCentro.Height div 2) - (dgMapa.Height div 2) - y;
end;

function TFrmMain.Jogar(const mapa: TMapa): Boolean;
begin
  Self.FMapa := mapa;
  lblTitulo.Caption := ExtractFileNameOnly(mapa.Arquivo);
  dgMapa.RowCount := mapa.Altura;
  dgMapa.ColCount := mapa.Largura;
  AjustaTela();
  CentralizarMapa();
  timerMain.Enabled := True;
  ShowModal();
  Result := Self.Mapa.Vitoria();
end;

end.

