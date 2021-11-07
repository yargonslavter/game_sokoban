unit ufrmrecursos;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFrmRecursos }

  TFrmRecursos = class(TForm)
    imgCaixa1: TImage;
    imgCaixa2: TImage;
    imgChao: TImage;
    imgObjetivo: TImage;
    imgParede: TImage;
    imgFundo: TImage;
    imgPersonagemBottom: TImage;
    imgPersonagemLeft: TImage;
    imgPersonagemRight: TImage;
    imgPersonagemTop: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
  private

  public
    procedure CarregarSprites();
  end;

var
  FrmRecursos: TFrmRecursos;

implementation

{$R *.lfm}

uses usokoban;

{ TFrmRecursos }

procedure TFrmRecursos.CarregarSprites();
begin
  // Carregar todos os sprites do jogo
  // Fundo
  imgFundo.Picture.LoadFromFile('./assets/fundo.jpg');
  // Chao
  imgChao.Picture.LoadFromFile('./assets/Chao.png');
  // Parede
  imgParede.Picture.LoadFromFile('./assets/Parede.png');
  // Personagem
  imgPersonagemTop.Picture.LoadFromFile('./assets/Personagem_Top.png');
  imgPersonagemBottom.Picture.LoadFromFile('./assets/Personagem_Bottom.png');
  imgPersonagemLeft.Picture.LoadFromFile('./assets/Personagem_Left.png');
  imgPersonagemRight.Picture.LoadFromFile('./assets/Personagem_Right.png');
  // Caixa
  imgCaixa1.Picture.LoadFromFile('./assets/Caixa1.png');
  imgCaixa2.Picture.LoadFromFile('./assets/Caixa2.png');
  // Objetivo
  imgObjetivo.Picture.LoadFromFile('./assets/Objetivo.png');



end;

end.

