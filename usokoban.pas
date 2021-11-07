unit usokoban;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  CO_DIR_MAPAS = 'maps';
  CO_DIR_ASSETS = 'assets';
  CO_ARQUIVO_RECORDES = './recordes.cfg';
  // EDITOR (C#)
  {
  co_free = 0;
  co_wall = 1;
  co_objective = 2;
  co_crate = 3;
  co_char = 4;
  co_pathused = 5;
  co_crateOverObjective = 6;
  co_charOverObjective = 7;
  }
  CO_JSON_VAZIO = '0';
  CO_JSON_PAREDE = '1';
  CO_JSON_OBJETIVO = '2';
  CO_JSON_CAIXA = '3';
  CO_JSON_PERSONAGEM = '4';
  CO_JSON_CAMINHO_USADO = '5';
  CO_JSON_CAIXA_OBJETIVO = '6';
  CO_JSON_PERSONAGEM_OBJETIVO = '7';

type
  { Declaração global }
  TMapa = class;

  { TDirecao }
  TDirecao = ( pdCima, pdBaixo, pdEsquerda, pdDireita );

  { TSokobanObj }
  TSokobanObj = class(TObject)

  end;

  { TObjetoMapa }
  TObjetoMapa = class(TSokobanObj)
  private
    FMapa: TMapa;
    FMovimentos: Integer;
    FNome: String;
    FX: Integer;
    FX_Inicial: Integer;
    FY: Integer;
    FY_Inicial: Integer;
  public
    constructor Create(const _mapa: TMapa); overload;
    constructor Create(const _mapa: TMapa; const _x, _y: Integer); overload;

    // Métodos
    procedure Resetar(); virtual;
    function ForaDoMapa(const direcao: TDirecao): Boolean; virtual;
    function TemChao(const direcao: TDirecao): Boolean; virtual;
    function TemParede(const direcao: TDirecao): Boolean; virtual;
    function TemCaixa(const direcao: TDirecao): Boolean; virtual;
    function TemObjetivo(const direcao: TDirecao): Boolean; virtual;
    function PodeMovimentar(const direcao: TDirecao): Boolean; virtual;
    function Mover(const direcao: TDirecao): Boolean; virtual;
    function ObjetivoAbaixo(): Boolean;

  published
    property Mapa: TMapa read FMapa;
    property X_Inicial: Integer read FX_Inicial;
    property Y_Inicial: Integer read FY_Inicial;
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
    property Nome: String read FNome write FNome;
    property Movimentos: Integer read FMovimentos;
  end;

  { TObjetivo }
  TObjetivo = class(TObjetoMapa)

  end;

  { TPersonagem }
  TPersonagem = class(TObjetoMapa)
  private
    FDirecao: TDirecao;

  public
    constructor Create(const _mapa: TMapa); overload;
    constructor Create(const _mapa: TMapa; const _x, _y: Integer); overload;

    // Methods
    function PodeMovimentar(const direcao: TDirecao): Boolean; override;
    function Mover(const direcao: TDirecao): Boolean; override;

    // Properties
    property Direcao: TDirecao read FDirecao write FDirecao;
  end;

  { TCaixa }
  TCaixa = class(TObjetoMapa)
  private

  public
  end;

  { TMapaRecorde }
  TMapaRecorde = class
  private
    FConcluido: Boolean;
    FMapa: TMapa;
    FMovimentos: Integer;
    FPassos: Integer;
    FTempo: Double;

  public
    constructor Create(_mapa: TMapa);

    function Carregar(): Boolean; overload;
    function Carregar(str: string): Boolean; overload;
    function Salvar(): Boolean;
    procedure Resetar();
    function TentarQuebrar(datahora: TDateTime; tempo: Double; passos, movimentos: Integer): Boolean;

    // Properiedades
    property Mapa: TMapa read FMapa;
    property Concluido: Boolean read FConcluido write FConcluido;
    property Tempo: Double read FTempo write FTempo;
    property Passos: Integer read FPassos write FPassos;
    property Movimentos: Integer read FMovimentos write FMovimentos;
  end;

  { TMapa }
  TMapa = class(TSokobanObj)
  private
    FArquivo: string;
    FAltura: Integer;
    FEstrutura: string;
    FLargura: Integer;
    // Listas
    FListaObjetivos: TList;
    FListaCaixas: TList;
    FPersonagem: TPersonagem;
    FRecorde: TMapaRecorde;
  public
    constructor Create();
    destructor Destroy(); override;

    // Métodos
    procedure LimparMapa();
    function Carregar(const arquivo: string): Boolean;
    procedure Resetar();
    function Vitoria(): Boolean;

    // Estrutura
    function PegarTile(const x, y: Integer): string;
    function EstaLivre(const x, y: Integer): Boolean;

    // Objetivos
    function TotalObjetivos(): Integer;
    function Objetivo(const idx: Integer): TObjetivo; overload;
    function Objetivo(const x, y: Integer): TObjetivo; overload;

    // Caixas
    function TotalMovimentoCaixas(): Integer;
    function TotalCaixas(): Integer;
    function Caixa(const idx: Integer): TCaixa; overload;
    function Caixa(const x, y: Integer): TCaixa; overload;

  public
    { JS }
    property Arquivo: string read FArquivo;
    property Estrutura: string read FEstrutura;
    property Largura: Integer read FLargura;
    property Altura: Integer read FAltura;
    property Personagem: TPersonagem read FPersonagem;
    { X }
    property Recorde: TMapaRecorde read FRecorde;
  end;

  { TMapaCollection }

  TMapaCollection = class(TSokobanObj)
   private
     FLista: TList;
     function GetTotal: Integer;
   public
     constructor Create();
     destructor Destroy(); override;

     //
     function Adicionar(var mapa: TMapa): Integer;
     function Remover(var mapa: TMapa): Boolean;
     function Obter(const indice: Integer): TMapa;

     property Total: Integer read GetTotal;
  end;

  { TLZSokoban }
  TLZSokoban = class(TSokobanObj)
  private
    FMapas_Classicos: TMapaCollection;
    FMapas_Extras: TMapaCollection;
    FMapas_Tutoriais: TMapaCollection;
  private
    procedure LimparListas();
  public
    constructor Create();
    destructor Destroy(); override;

    function CarregarMapas(): Boolean;
  public
    property Mapas_Tutoriais: TMapaCollection read FMapas_Tutoriais;
    property Mapas_Classicos: TMapaCollection read FMapas_Classicos;
    property Mapas_Extras: TMapaCollection read FMapas_Extras;
  end;


var
  Sokoban: TLZSokoban;

implementation

uses LazFileUtils, FileUtil, fpjson, jsonparser, jsonscanner, strutils;

{ TMapaRecorde }

constructor TMapaRecorde.Create(_mapa: TMapa);
begin
  FMapa := _mapa;
end;

function TMapaRecorde.Carregar(): Boolean;
var
  f: TStringList;
  s: string;
begin
  Result := False;

  if not Assigned(FMapa) then
    Exit;

  if FileExists(CO_ARQUIVO_RECORDES) then
  begin
    // O arquivo existe, carregar dos recordes
    s := '';
    f := TStringList.Create();
    try
      try
        f.LoadFromFile(CO_ARQUIVO_RECORDES);
        s := f.Values[FMapa.Arquivo];
      except
        // Ignorar exception
      end;
    finally
      f.Free();
    end;
    Result := Self.Carregar(s);
  end
  else
  begin
    // Não existe o arquivo de recordes, então zerar tudo
    Self.Concluido := False;
    Self.Passos := 0;
    Self.Movimentos := 0;
    Self.Tempo := 0;
    Result := True;
  end;
end;

function TMapaRecorde.Carregar(str: string): Boolean;
var
  strl: TStringList;
  d: Double;
  i: Integer;
begin
  Result := False;

  if CompareStr(str, EmptyStr) <> 0 then
  begin
    str := ReplaceStr(str, ':', '=');
    str := ReplaceStr(str, ';', sLineBreak);

    strl := TStringList.Create();
    try
      strl.Text := str;

      // Tempo
      if TryStrToFloat(strl.Values['T'], d) then
        Self.Tempo := d
      else
        Self.Tempo := 0;
      // Passos
      if TryStrToInt(strl.Values['P'], i) then
        Self.Passos := i
      else
        Self.Passos := 0;
      // Movimentos
      if TryStrToInt(strl.Values['M'], i) then
        Self.Movimentos := i
      else
        Self.Movimentos := 0;
      Self.Concluido := Self.Tempo > 0;
    finally
      strl.Free();
    end;
  end
  else
  begin
    Self.Concluido := False;
    Self.Tempo := 0;
    Self.Passos := 0;
    Self.Movimentos := 0;
  end;

  Result := True;
end;

function TMapaRecorde.Salvar(): Boolean;
var
  s: string;
  f: TStringList;
begin
  Result := False;
  f := TStringList.Create();
  try
    if FileExists(CO_ARQUIVO_RECORDES) then
      f.LoadFromFile(CO_ARQUIVO_RECORDES);

    if Self.Concluido then
      s := Format('T:%f;M:%d;P:%d', [Self.Tempo, Self.Movimentos, Self.Passos])
    else
      s := '';
    f.Values[FMapa.Arquivo] := s;
    try
      f.SaveToFile(CO_ARQUIVO_RECORDES);
      Result := True;
    except
    end;
  finally
    f.Free();
  end;
end;

procedure TMapaRecorde.Resetar();
begin
  Self.Carregar(EmptyStr);
end;

function TMapaRecorde.TentarQuebrar(datahora: TDateTime; tempo: Double; passos,
  movimentos: Integer): Boolean;
begin
  Result := False;

  if Self.Concluido then
  begin
    if tempo < Self.Tempo then
    begin
      Self.Tempo := tempo;
      Result := True;
    end;
    if passos < Self.Passos then
    begin
      Self.Passos := passos;
      Result := True;
    end;
    if movimentos < Self.Movimentos then
    begin
      Self.Movimentos := movimentos;
      Result := True;
    end;
    if Result then
    begin
      Self.Concluido := True;
    end;
  end
  else
  begin
    Self.Concluido := True;
    Self.Tempo := tempo;
    Self.Passos := passos;
    Self.Movimentos := movimentos;
    Result := True;
  end;

  if Result then
  begin
    Self.Salvar();
  end;
end;

{ TPersonagem }

constructor TPersonagem.Create(const _mapa: TMapa);
begin
  inherited Create(_mapa);
  FDirecao := TDirecao.pdBaixo;
end;

constructor TPersonagem.Create(const _mapa: TMapa; const _x, _y: Integer);
begin
  inherited Create(_mapa, _x, _y);
  FDirecao := TDirecao.pdBaixo;
end;

function TPersonagem.PodeMovimentar(const direcao: TDirecao): Boolean;
var
  caixa: TCaixa;
begin
  Result := False;

  if ForaDoMapa(direcao) then
    Exit;

  if not TemChao(direcao) then
    Exit;

  if TemParede(direcao) then
    Exit;

  // Verificar se pode mover a caixa
  caixa := nil;
  case direcao of
    pdCima: caixa := Self.Mapa.Caixa(Self.X, Self.Y - 1);
    pdBaixo: caixa := Self.Mapa.Caixa(Self.X, Self.Y + 1);
    pdEsquerda: caixa := Self.Mapa.Caixa(Self.X - 1, Self.Y);
    pdDireita: caixa := Self.Mapa.Caixa(Self.X + 1, Self.Y);
  end;
  if Assigned(caixa) and not (caixa.PodeMovimentar(direcao)) then
    Exit;

  // OK
  Result := True;
end;

function TPersonagem.Mover(const direcao: TDirecao): Boolean;
var
  caixa: TCaixa;
begin
  Self.Direcao := direcao;

  Result := inherited Mover(direcao);

  // Verificar se há uma caixa abaixo do jogador
  caixa := Self.Mapa.Caixa(Self.X, Self.Y);
  if Assigned(caixa) then
    caixa.Mover(direcao);
end;

{ TMapaCollection }

function TMapaCollection.GetTotal: Integer;
begin
  Result := FLista.Count;
end;

constructor TMapaCollection.Create();
begin
  FLista := TList.Create();
end;

destructor TMapaCollection.Destroy();
begin
  inherited Destroy();
  FLista.Free();;
end;

function TMapaCollection.Adicionar(var mapa: TMapa): Integer;
begin
  Result := FLista.Add(mapa);
end;

function TMapaCollection.Remover(var mapa: TMapa): Boolean;
begin
  Result := FLista.Remove(mapa) <> -1;
end;

function TMapaCollection.Obter(const indice: Integer): TMapa;
begin
  Result := TMapa(FLista.Items[indice]);
end;

{ TLZSokoban }

procedure TLZSokoban.LimparListas();
begin
  // Limpar collections
end;

constructor TLZSokoban.Create();
begin
  FMapas_Tutoriais := TMapaCollection.Create();
  FMapas_Classicos := TMapaCollection.Create();
  FMapas_Extras := TMapaCollection.Create();
end;

destructor TLZSokoban.Destroy();
begin
  FMapas_Tutoriais.Free();
  FMapas_Classicos.Free();
  FMapas_Extras.Free();
  inherited Destroy();
end;

function TLZSokoban.CarregarMapas(): Boolean;
var
   list: TStringList;
   i: Integer;
   s: string;
   map: TMapa;
begin
  Result := False;

  // Testar se existe  a pasta de mapas
  if not DirectoryExists(CO_DIR_MAPAS) then
  begin
    // Tentar criar a pasta
    if not CreateDir(CO_DIR_MAPAS) then
       Exit;
  end;

  // Resetar listas
  LimparListas();

  // Listar arquivos da pasta
  list := FindAllFiles(CO_DIR_MAPAS);
  try
    list.Sort();

    // Verificar os mapas carregados
    if list.Count > 0 then
    begin

      // Adicionar como menus
      for i := 0 to list.Count - 1 do
      begin
        s := list[i];

        // Só processar se o arquivo for um JSON
        if not AnsiEndsText('.json', s) then
          Continue;

        // Carregar mapa
        map := TMapa.Create();
        try
          // Carregar mapa do arquivo
          map.Carregar(s);
        except
          // Falha ao carregar o mapa
          map.Free;

          // Pular esse arquivo
          Continue;
        end;

        // Encaixar na lista respectiva
        s := ExtractFileNameOnly(s);

        if AnsiStartsText('tut', s) then
        begin
          // Tutoriais
          FMapas_Tutoriais.Adicionar(map);
        end
        else if AnsiStartsText('Classic', s) then
        begin
          // Clássicos
          FMapas_Classicos.Adicionar(map);
        end
        else
        begin
          // Extras
          FMapas_Extras.Adicionar(map);
        end;
      end;

    end;

  finally
    // Destroy list
    list.Free;
  end;

end;

{ TObjetoMapa }

constructor TObjetoMapa.Create(const _mapa: TMapa);
begin
  Create(_mapa, 0, 0);
end;

constructor TObjetoMapa.Create(const _mapa: TMapa; const _x, _y: Integer);
begin
  FMapa := _mapa;
  FX_Inicial := _x;
  FY_Inicial := _y;
  FX := _x;
  FY := _y;
end;

procedure TObjetoMapa.Resetar();
begin
  // Resetar
  FX := FX_Inicial;
  FY := FY_Inicial;
  FMovimentos := 0;
end;

function TObjetoMapa.ForaDoMapa(const direcao: TDirecao): Boolean;
begin
  Result := ((direcao = pdCima) and (Self.Y <= 0)) or ((direcao = pdBaixo) and (Self.Y >= Self.Mapa.Altura - 1)) or
     ((direcao = pdEsquerda) and (Self.X <= 0)) or ((direcao = pdDireita) and (Self.X >= Self.Mapa.Largura - 1));
end;

function TObjetoMapa.TemChao(const direcao: TDirecao): Boolean;
var
   s: string;
begin
  s := '';
  case direcao of
    pdCima:
    begin
      s := Self.Mapa.PegarTile(Self.X, Self.Y - 1);
    end;
    pdBaixo:
    begin
      s := Self.Mapa.PegarTile(Self.X, Self.Y + 1);
    end;
    pdEsquerda:
    begin
      s := Self.Mapa.PegarTile(Self.X - 1, Self.Y);
    end;
    pdDireita:
    begin
      s := Self.Mapa.PegarTile(Self.X + 1, Self.Y);
    end;
  end;

  Result := s = CO_JSON_VAZIO;
end;

function TObjetoMapa.TemParede(const direcao: TDirecao): Boolean;
var
   s: string;
begin
  s := '';
  case direcao of
    pdCima:
    begin
      s := Self.Mapa.PegarTile(Self.X, Self.Y - 1);
    end;
    pdBaixo:
    begin
      s := Self.Mapa.PegarTile(Self.X, Self.Y + 1);
    end;
    pdEsquerda:
    begin
      s := Self.Mapa.PegarTile(Self.X - 1, Self.Y);
    end;
    pdDireita:
    begin
      s := Self.Mapa.PegarTile(Self.X + 1, Self.Y);
    end;
  end;

  Result := s = CO_JSON_PAREDE;
end;

function TObjetoMapa.TemCaixa(const direcao: TDirecao): Boolean;
var
   caixa: TCaixa;
begin
  caixa := nil;
  case direcao of
    pdCima:
    begin
      caixa := Self.Mapa.Caixa(Self.X, Self.Y - 1);
    end;
    pdBaixo:
    begin
      caixa := Self.Mapa.Caixa(Self.X, Self.Y + 1);
    end;
    pdEsquerda:
    begin
      caixa := Self.Mapa.Caixa(Self.X - 1, Self.Y);
    end;
    pdDireita:
    begin
      caixa := Self.Mapa.Caixa(Self.X + 1, Self.Y);
    end;
  end;

  Result := Assigned(caixa);
end;

function TObjetoMapa.TemObjetivo(const direcao: TDirecao): Boolean;
var
   obj: TObjetivo;
begin
  obj := nil;
  case direcao of
    pdCima:
    begin
      obj := Self.Mapa.Objetivo(Self.X, Self.Y - 1);
    end;
    pdBaixo:
    begin
      obj := Self.Mapa.Objetivo(Self.X, Self.Y + 1);
    end;
    pdEsquerda:
    begin
      obj := Self.Mapa.Objetivo(Self.X - 1, Self.Y);
    end;
    pdDireita:
    begin
      obj := Self.Mapa.Objetivo(Self.X + 1, Self.Y);
    end;
  end;

  Result := Assigned(obj);
end;

function TObjetoMapa.PodeMovimentar(const direcao: TDirecao): Boolean;
begin
  Result := False;

  // Verificar está indo pra forda da borda
  if ForaDoMapa(direcao) then
    Exit;

  // Verificar se é chão
  if not Self.TemChao(direcao) then
    Exit;

  // Verificar se tem alguma caixa no caminho
  if Self.TemCaixa(direcao) then
    Exit;

  // Tudo OK
  Result := True;
end;

function TObjetoMapa.Mover(const direcao: TDirecao): Boolean;
begin
  Result := False;

  if not Self.PodeMovimentar(direcao) then
    Exit;

  // Tudo OK, mover
  Result := True;
  case direcao of
    pdCima:
    begin
      Self.Y :=  Self.Y - 1;
    end;
    pdBaixo:
    begin
      Self.Y :=  Self.Y + 1;
    end;
    pdEsquerda:
    begin
      Self.X :=  Self.X - 1;
    end;
    pdDireita:
    begin
      Self.X :=  Self.X + 1;
    end;
  end;
  Inc(FMovimentos);
end;

function TObjetoMapa.ObjetivoAbaixo(): Boolean;
var
   obj: TObjetivo;
begin
  Result := False;

  if not Assigned(Self.Mapa) then
    Exit;

  obj := Self.Mapa.Objetivo(Self.X, Self.Y);
  if not Assigned(obj) then
    Exit;

  Result := True;
end;

{ TMapa }

constructor TMapa.Create();
begin
  FListaObjetivos := TList.Create();
  FListaCaixas := TList.Create();
end;

destructor TMapa.Destroy();
begin
  LimparMapa();

  FreeAndNil(FListaObjetivos);

  FreeAndNil(FListaCaixas);

  if Assigned(FPersonagem) then
    FreeAndNil(FPersonagem);

  inherited;
end;

procedure TMapa.LimparMapa();
var
  i: Integer;
begin
  FEstrutura := '';

  // Limpar objetivos
  for i := 0 to Pred(TotalObjetivos) do
  begin
    Objetivo(i).Free();
  end;
  FListaObjetivos.Clear();

  // Limpar caixas
  for i := 0 to Pred(TotalCaixas) do
  begin
    Caixa(i).Free();
  end;
  FListaCaixas.Clear();
end;

function TMapa.Carregar(const arquivo: string): Boolean;
var
  f: TFileStream = nil;
  json: TJSONParser = nil;
  data: TJSONData;
  jsobj: TJSONObject;
  i, n: Integer;
  jsarr: TJSONArray;
  str: string;
  obj: TObjetivo;
  cx: TCaixa;
begin
  Result := False;
  LimparMapa();

  // Load MAP from File
  try
    // Load File to memory
    f := TFileStream.Create(arquivo, fmOpenRead);

    // JSON Parser
    json := TJSONParser.Create(f, [joUTF8]);

    // Data
    data := json.Parse();
    jsobj := TJSONObject(data);

    // Load Data
    FArquivo := arquivo;
    FLargura := jsobj.Find('Width').AsInteger;
    FAltura := jsobj.Find('Height').AsInteger;

    // Limpar o mapa da memória
    FEstrutura := '';

    // Carregar mapa pra memória
    data := jsobj.Find('Matrix');
    for i := 0 to Pred(data.Count) do
    begin
      jsarr := data.Items[i] as TJSONArray;
      for n := 0 to Pred(jsarr.Count) do
      begin
        // Verificar qual tile é
        str := jsarr.Strings[n];
        if Length(str) <> 1 then
        begin
          str := CO_JSON_VAZIO;
        end;

        // Processar Tile
        if str = CO_JSON_VAZIO then
        begin
          // Tile vazio, chão
        end
        else if str = CO_JSON_PAREDE then
        begin
          // Tile de parede
        end
        else if str = CO_JSON_OBJETIVO then
        begin
          // Tile de objetivo
          str := CO_JSON_VAZIO;

          obj := TObjetivo.Create(Self, i, n);
          obj.Nome := 'Objetivo ' + IntToStr(FListaObjetivos.Count);
          FListaObjetivos.Add(obj);
        end
        else if str = CO_JSON_CAIXA then
        begin
          // Tile de caixa
          str := CO_JSON_VAZIO;

          cx := TCaixa.Create(Self, i, n);
          cx.Nome := 'Caixa ' + IntToStr(FListaCaixas.Count);
          FListaCaixas.Add(cx);
        end
        else if str = CO_JSON_PERSONAGEM then
        begin
          // Tile de personagem
          str := CO_JSON_VAZIO;

          FPersonagem := TPersonagem.Create(Self, i, n);
          FPersonagem.Nome := 'Jogador';
        end
        else if str = CO_JSON_CAMINHO_USADO then
        begin
          // Tile de chão
          str := CO_JSON_VAZIO;
        end
        else if str = CO_JSON_CAIXA_OBJETIVO then
        begin
          // Tile de caixa em cima do objetivo
          str := CO_JSON_VAZIO;

          obj := TObjetivo.Create(Self, i, n);
          obj.Nome := 'Objetivo ' + IntToStr(FListaObjetivos.Count);
          FListaObjetivos.Add(obj);

          cx := TCaixa.Create(Self, i, n);
          cx.Nome := 'Caixa ' + IntToStr(FListaCaixas.Count);
          FListaCaixas.Add(cx);
        end
        else if str = CO_JSON_PERSONAGEM_OBJETIVO then
        begin
          // Tile de personagem em cima do objetivo
          str := CO_JSON_VAZIO;

          FPersonagem := TPersonagem.Create(Self, i, n);
          FPersonagem.Nome := 'Jogador';

          obj := TObjetivo.Create(Self, i, n);
          obj.Nome := 'Objetivo ' + IntToStr(FListaObjetivos.Count);
          FListaObjetivos.Add(obj);
        end
        else
        begin
          Result := False;
          Exit;
        end;

        // Adicionar tile a estrutura
        FEstrutura := FEstrutura + str;
      end;
    end;

    // Recorde
    if Assigned(Self.FRecorde) then
      FreeAndNil(Self.FRecorde);

    Self.FRecorde := TMapaRecorde.Create(Self);
    Self.FRecorde.Carregar();

    Result := True;

  finally
    // Free memory
    if Assigned(f) then
      f.Free;
    if Assigned(json) then
      json.Free;
  end;
end;

procedure TMapa.Resetar();
var
  i: Integer;
begin
  // Resetar tudo
  Self.Personagem.Resetar();
  for i := 0 to Pred(Self.TotalCaixas) do
    Self.Caixa(i).Resetar();
  for i := 0 to Pred(Self.TotalObjetivos) do
    Self.Objetivo(i).Resetar();
end;

function TMapa.Vitoria(): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Pred(Self.TotalCaixas()) do
  begin
    if not Self.Caixa(i).ObjetivoAbaixo() then
      Exit;
  end;

  Result := True;
end;

function TMapa.PegarTile(const x, y: Integer): string;
var
  idx: Integer;
begin
  Result := '';
  //Result := CO_JSON_VAZIO;

  idx := (x * Altura) + y; // Está invertido porque o editor salva invertido :(
  if (idx >= 0) and (idx < Length(Estrutura)) then
    Result := Estrutura[idx + 1];
end;

function TMapa.EstaLivre(const x, y: Integer): Boolean;
begin
  Result := (Self.PegarTile(x, y) = CO_JSON_VAZIO);
end;

function TMapa.TotalObjetivos(): Integer;
begin
  Result := FListaObjetivos.Count;
end;

function TMapa.Objetivo(const idx: Integer): TObjetivo;
begin
  if (idx >= 0) and (idx < FListaObjetivos.Count) then
    Result := TObjetivo(FListaObjetivos.Items[idx])
  else
    Result := nil;
end;

function TMapa.Objetivo(const x, y: Integer): TObjetivo;
var
  i: Integer;
  obj: TObjetivo;
begin
  Result := nil;
  for i := 0 to Pred(TotalObjetivos) do
  begin
    obj := Objetivo(i);
    if not Assigned(obj) then
      Continue; // Nunca deveria chegar aqui

    if (obj.X = x) and (obj.Y = y) then
    begin
      Result := obj;
      Exit;
    end;
  end;
end;

function TMapa.TotalMovimentoCaixas(): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to Pred(Self.TotalCaixas()) do
  begin
    Result := Result + Self.Caixa(i).Movimentos;
  end;
end;

function TMapa.TotalCaixas(): Integer;
begin
  Result := FListaCaixas.Count;
end;

function TMapa.Caixa(const idx: Integer): TCaixa;
begin
  if (idx >= 0) and (idx < FListaObjetivos.Count) then
    Result := TCaixa(FListaCaixas.Items[idx])
  else
    Result := nil;
end;

function TMapa.Caixa(const x, y: Integer): TCaixa;
var
  i: Integer;
  cx: TCaixa;
begin
  Result := nil;
  for i := 0 to Pred(TotalCaixas) do
  begin
    cx := Caixa(i);
    if not Assigned(cx) then
      Continue; // Nunca deveria chegar aqui

    if (cx.X = x) and (cx.Y = y) then
    begin
      Result := cx;
      Exit;
    end;
  end;
end;


end.

