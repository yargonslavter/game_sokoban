unit ufrmescolhermapa;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, usokoban;

type

  { TFrmEscolherMapa }

  TFrmEscolherMapa = class(TForm)
    btnJogar: TButton;
    btnCancelar: TButton;
    imgMapa: TImageList;
    imgPrevia: TImage;
    lbxClasseMapas: TListBox;
    imgFundo: TImage;
    lvMapas: TListView;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    spB: TShape;
    spL: TShape;
    spR: TShape;
    spT: TShape;
    procedure btnJogarClick(Sender: TObject);
    procedure btnCancelarClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbxClasseMapasSelectionChange(Sender: TObject; User: boolean);
    procedure lstTutoriaisChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvMapasKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure lvMapasSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure pcPrincipalChange(Sender: TObject);
  private
    procedure AtualizaMapas();
    procedure AtualizaPrevia();
    procedure AtualizaSelecionado();
    function MapaSelecionado(): TMapa;
  public

  end;

var
  FrmEscolherMapa: TFrmEscolherMapa;

implementation

{$R *.lfm}

uses LazFileUtils, LCLType, UFrmMain, ufrmrecursos;

{ TFrmEscolherMapa }

procedure TFrmEscolherMapa.FormCreate(Sender: TObject);
begin
  // OnCreate

  // Imagem de fundo
  imgFundo.Picture.Assign(FrmRecursos.imgFundo.Picture);

  // Carregar mapas na UI
  AtualizaMapas();
end;

procedure TFrmEscolherMapa.FormShow(Sender: TObject);
begin
  // Primeiro selecionado
  if (lvMapas.SelCount = 0) then
    lvMapas.Items[0].Selected := True;
end;

procedure TFrmEscolherMapa.lbxClasseMapasSelectionChange(Sender: TObject; User: boolean);
begin
  AtualizaMapas();

  AtualizaPrevia();
end;

procedure TFrmEscolherMapa.lstTutoriaisChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  AtualizaPrevia();
end;

procedure TFrmEscolherMapa.lvMapasKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  m: TMapa;
begin
  case Key of
    VK_DELETE:
    begin
      if ssShift in Shift then
      begin
        if MessageDlg('Aviso', 'Deseja realmente excluir o recorde atual?', mtWarning, [mbYes, mbNo], 0) = mrYes then
        begin
          m := MapaSelecionado();
          if not Assigned(m) then
            Exit;
          m.Recorde.Resetar();
          m.Recorde.Salvar();
          Self.AtualizaSelecionado();
        end;
      end;
    end;
    VK_RETURN:
    begin
      btnJogar.Click();
    end;
  end;
end;

procedure TFrmEscolherMapa.lvMapasSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  AtualizaPrevia();
end;

procedure TFrmEscolherMapa.pcPrincipalChange(Sender: TObject);
begin
  AtualizaPrevia();
end;

procedure TFrmEscolherMapa.AtualizaMapas();
var
  i: Integer;
  mapa: TMapa;

  procedure adicionarMapa(m: TMapa);
  var
    item: TListItem;
  begin
    item := lvMapas.Items.Add();
    item.Data := m;
    item.Caption := ExtractFileNameOnly(m.Arquivo);
    if m.Recorde.Concluido then
    begin
      item.ImageIndex := 1;
      item.SubItems.Add(IntToStr(m.Recorde.Passos));
      item.SubItems.Add(IntToStr(m.Recorde.Movimentos));
      item.SubItems.Add(FormatFloat('0.0', m.Recorde.Tempo) + 's');
    end
    else
    begin
      item.ImageIndex := 0;
      item.SubItems.Add('-');
      item.SubItems.Add('-');
      item.SubItems.Add('-');
    end;
  end;

begin
  lvMapas.Clear();

  case lbxClasseMapas.ItemIndex of
    0: begin
      // Tutoriais
      for i := 0 to Pred(Sokoban.Mapas_Tutoriais.Total) do
      begin
        mapa := Sokoban.Mapas_Tutoriais.Obter(i);
        adicionarMapa(mapa);
      end;
    end;
    1: begin
      // Clássicas
      for i := 0 to Pred(Sokoban.Mapas_Classicos.Total) do
      begin
        mapa := Sokoban.Mapas_Classicos.Obter(i);
        adicionarMapa(mapa);
      end;
    end;
    2: begin
      // Adicionais
      for i := 0 to Pred(Sokoban.Mapas_Extras.Total) do
      begin
        mapa := Sokoban.Mapas_Extras.Obter(i);
        adicionarMapa(mapa);
      end;
    end
    else
    begin
      // ?
    end;
  end;

  // Primeiro selecionado
  if (lvMapas.SelCount = 0) then
    lvMapas.Items[0].Selected := True;

end;

procedure TFrmEscolherMapa.AtualizaPrevia();
var
  mapa: TMapa;
  bmp: TBitmap;
  n, x, y: Integer;
  s: string;
  r: TRect;
  obj: TObjetivo;
  caixa: TCaixa;
begin
  mapa := Self.MapaSelecionado();
  if Assigned(mapa) then
  begin
    // Calcular o tamanho ideal dos tiles para mostrar a prévia
    if (mapa.Largura >= mapa.Altura) then
      n := Trunc(imgPrevia.Width / mapa.Largura)
    else
      n := Trunc(imgPrevia.Height / mapa.Altura);

    // Montar a prévia
    bmp := TBitmap.Create();
    bmp.SetSize(mapa.Largura * n, mapa.Altura * n);

    // Chão e parede
    for x := 0 to Pred(mapa.Largura) do
    begin
      for y := 0 to Pred(mapa.Altura) do
      begin
        s := mapa.PegarTile(x, y);
        r := Bounds(x * n, y * n, n, n);

        if (s = CO_JSON_VAZIO) then
          bmp.Canvas.StretchDraw(r, FrmRecursos.imgChao.Picture.Graphic)
        else if (s = CO_JSON_PAREDE) then
          bmp.Canvas.StretchDraw(r, FrmRecursos.imgParede.Picture.Graphic)
        else
        begin
          bmp.Canvas.Brush.Color := clBlue;
          bmp.Canvas.FillRect(r);
        end;
      end;
    end;

    // Objetivos
    for x := 0 to Pred(mapa.TotalObjetivos) do
    begin
      obj := mapa.Objetivo(x);
      r := Bounds(obj.X * n, obj.Y * n, n, n);
      bmp.Canvas.StretchDraw(r, FrmRecursos.imgObjetivo.Picture.Graphic);
    end;

    // Caixas
    for x := 0 to Pred(mapa.TotalCaixas) do
    begin
      caixa := mapa.Caixa(x);
      r := Bounds(caixa.X * n, caixa.Y * n, n, n);
      if not caixa.ObjetivoAbaixo() then
        bmp.Canvas.StretchDraw(r, FrmRecursos.imgCaixa1.Picture.Graphic)
      else
        bmp.Canvas.StretchDraw(r, FrmRecursos.imgCaixa2.Picture.Graphic);
    end;

    // Personagem
    r := Bounds(mapa.Personagem.X * n, mapa.Personagem.Y * n, n, n);
    bmp.Canvas.StretchDraw(r, FrmRecursos.imgPersonagemBottom.Picture.Graphic);

    // Mostrar
    imgPrevia.Picture.Assign(bmp);
    bmp.Free();
  end
  else
  begin
    imgPrevia.Picture.Assign(nil);
  end;

end;

procedure TFrmEscolherMapa.AtualizaSelecionado();
var
  item: TListItem;
  mapa: TMapa;
begin
  item := lvMapas.Selected;
  if not Assigned(item) then
    Exit;

  mapa := TMapa(item.Data);
  if not Assigned(mapa) then
    Exit;

  item.Caption := ExtractFileNameOnly(mapa.Arquivo);
  item.SubItems.Clear();
  if mapa.Recorde.Concluido then
  begin
    item.ImageIndex := 1;
    item.SubItems.Add(IntToStr(mapa.Recorde.Passos));
    item.SubItems.Add(IntToStr(mapa.Recorde.Movimentos));
    item.SubItems.Add(FormatFloat('0.0', mapa.Recorde.Tempo) + ' s');
  end
  else
  begin
    item.ImageIndex := 0;
    item.SubItems.Add('-');
    item.SubItems.Add('-');
    item.SubItems.Add('-');
  end;
end;

function TFrmEscolherMapa.MapaSelecionado(): TMapa;
begin
  Result := nil;

  if lvMapas.ItemIndex >= 0 then
    Result := TMapa(lvMapas.Items[lvMapas.ItemIndex].Data);
end;

procedure TFrmEscolherMapa.btnJogarClick(Sender: TObject);
var
  mapa: TMapa;
begin
  // Jogar mapa selecionada
  mapa := MapaSelecionado();
  if Assigned(mapa) then
  try
    Hide();
    FrmMain := TFrmMain.Create(Application);
    if FrmMain.Jogar(mapa) then
    begin
      mapa.Resetar();
      Self.AtualizaPrevia();
      Self.AtualizaSelecionado();
    end;
  finally
    Show();
    BringToFront();
    lvMapas.SetFocus();
  end;

end;

procedure TFrmEscolherMapa.btnCancelarClick(Sender: TObject);
begin
  // Fechar
  Close();
end;

end.

