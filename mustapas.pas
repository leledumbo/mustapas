unit mustapas;

{$mode objfpc}{$H+}

interface

uses
  Classes,SysUtils,fpjson;

type
  TCacheGetFunction = function (const Template: TStream; const Context: TJSONObject; Result: TStream): Boolean;
  TCachePutFunction = procedure (const Template: TStream; const Context: TJSONObject; const Result: TStream);

  ETokenize = class(Exception) end;
  ERender = class(Exception) end;

var
  OnGetCache: TCacheGetFunction;
  OnPutCache: TCachePutFunction;

function Escape(const S: String): String; inline;
procedure Render(const Template: TStream; const Context: TJSONObject; Result: TStream);

implementation

uses
  StrUtils,StreamIO;

function Escape(const S: String): String;
begin
  Result := StringsReplace(S,[#13,#10],['\r','\n'],[rfReplaceAll]);
end;

{ token }

type
  TTokenKind = (
    tkText,tkEscapedVariable,tkUnescapedVariable,tkOpenSection,tkCloseSection
  );

  PToken = ^TToken;
  TToken = record
    Line,Col: LongWord;
    Kind: TTokenKind;
    Lexeme: String;
    ContextBasePath: String;
  end;

  TTokenList = array of TToken;

procedure AddToken(const ALine,ACol: LongWord; const AKind: TTokenKind;
  const ALexeme,AContextBasePath: String; var Tokens: TTokenList);
var
  n: SizeInt;
  t: TToken;
begin
  // WriteLn('Token ',AKind,' of "',Escape(ALexeme),'" has been added');
  with t do begin
    Line := ALine;
    Col := ACol;
    Kind := AKind;
    Lexeme := ALexeme;
    ContextBasePath := AContextBasePath;
  end;
  n := Length(Tokens);
  SetLength(Tokens,n + 1);
  Tokens[n] := t;
end;

{ lexer }

const
  EOFChar     = #26;
  LFChar      = #10;

type
  TLexerState = (
    lsText,lsPossiblyTagStart,lsTagStart,lsPossiblyTagEnd,lsTagEnd,
    lsEscapedVariable,lsUnescapedVariable,lsPossiblyUnescapedVariableEnd,lsUnescapedVariableAmp,
    lsOpenSection,lsCloseSection,
    lsEOF
  );

var
  Input: Text;
  LexerState: TLexerState;
  Line,Col: LongWord; // current lexer position
  LineBuf: String;    // line buffer, holds current line contents of the source file

function ReadChar: Char; // read next character from the input stream
begin
  if EOF(Input) then begin
    Result := EOFChar;
  end else begin
    Read(Input,Result);
    if Result = LFChar then begin
      Inc(Line);
      Col := 0;
    end else begin
      Inc(Col);
    end;
  end;
end;

function Tokenize: TTokenList;
var
  StartLine,StartCol: LongWord;
  Kind: TTokenKind;
  Lexeme,ContextBasePath: String;
  Look: Char;
  TempState: TLexerState;

  procedure InitFields;
  begin
    StartLine := Line;
    StartCol := Col;
    Lexeme := EmptyStr;
  end;

  procedure HandleText;
  begin
    case Look of
      '{': begin
        LexerState := lsPossiblyTagStart;
      end;
      else begin
        Lexeme := Lexeme + Look;
      end;
    end;
    Look := ReadChar;
  end;

  procedure HandlePossiblyTagStart;
  begin
    case Look of
      '{': begin
        if Lexeme <> EmptyStr then begin
          AddToken(StartLine,StartCol,Kind,Lexeme,ContextBasePath,Result);
          InitFields;
        end;
        LexerState := lsTagStart;
      end;
      else begin
        Lexeme := Lexeme + '{' + Look;
        LexerState := lsText;
      end;
    end;
    Look := ReadChar;
  end;

  procedure HandleTagStart;
  begin
    case Look of
      '{': begin
        LexerState := lsUnescapedVariable;
        Kind := tkUnescapedVariable;
      end;
      '&': begin
        LexerState := lsUnescapedVariableAmp;
        Kind := tkUnescapedVariable;
      end;
      '#': begin
        LexerState := lsOpenSection;
        Kind := tkOpenSection;
      end;
      '/': begin
        LexerState := lsCloseSection;
        Kind := tkCloseSection;
      end;
      'a'..'z','A'..'Z','_',' ': begin
        LexerState := lsEscapedVariable;
        Kind := tkEscapedVariable;
        Lexeme := Look;
      end;
      else begin
        raise ETokenize.Create('Invalid tag char: ' + Look);
      end;
    end;
    Look := ReadChar;
  end;

  procedure HandlePossiblyTagEnd;
  var
    p: SizeInt;
  begin
    case Look of
      '}': begin
        Lexeme := Trim(Lexeme);
        case TempState of
          lsOpenSection: ContextBasePath := ContextBasePath + Lexeme + '.';
          lsCloseSection: begin
            if ContextBasePath = EmptyStr then
              raise Exception.Create('Closing unopened section: ' + Lexeme)
            else begin
              p := RPos('.',ContextBasePath);
              Delete(ContextBasePath,p,Length(ContextBasePath) - p);
            end;
          end;
        end;
        AddToken(StartLine,StartCol,Kind,Lexeme,ContextBasePath,Result);
        InitFields;
        LexerState := lsTagEnd;
      end;
      else begin
        Lexeme := Lexeme + '}' + Look;
        LexerState := TempState;
      end;
    end;
    Look := ReadChar;
  end;

  procedure HandleTagEnd;
  begin
    LexerState := lsText;
    Kind := tkText;
  end;

  procedure HandleVariable;
  begin
    case Look of
      '}': begin
        TempState := LexerState;
        if LexerState = lsEscapedVariable then
          LexerState := lsPossiblyTagEnd
        else
          LexerState := lsPossiblyUnescapedVariableEnd;
      end;
      'a'..'z','A'..'Z','0'..'9','_',' ','.': begin
        Lexeme := Lexeme + Look;
      end;
      else begin
        raise ETokenize.Create('Invalid tag char: ' + Look);
      end;
    end;
    Look := ReadChar;
  end;

  procedure HandlePossiblyUnescapedVariableEnd;
  begin
    case Look of
      '}': begin
        LexerState := lsPossiblyTagEnd;
      end;
      else begin
        raise ETokenize.Create('Invalid tag char: ' + Look);
      end;
    end;
    Look := ReadChar;
  end;

  procedure HandleUnescapedVariableAmp;
  begin
    case Look of
      '}': begin
        TempState := LexerState;
        LexerState := lsPossiblyTagEnd;
      end;
      'a'..'z','A'..'Z','0'..'9','_',' ','.': begin
        Lexeme := Lexeme + Look;
      end;
      else begin
        raise ETokenize.Create('Invalid tag char: ' + Look);
      end;
    end;
    Look := ReadChar;
  end;

  procedure HandleSection;
  begin
    case Look of
      '}': begin
        TempState := LexerState;
        LexerState := lsPossiblyTagEnd;
      end;
      'a'..'z','A'..'Z','0'..'9','_',' ','.': begin
        Lexeme := Lexeme + Look;
      end;
      else begin
        raise ETokenize.Create('Invalid tag char: ' + Look);
      end;
    end;
    Look := ReadChar;
  end;

begin
  Initialize(Result);
  LexerState := lsText;

  InitFields;
  ContextBasePath := EmptyStr;
  Look := ReadChar;
  Kind := tkText;
  while Look <> EOFChar do begin
    case LexerState of
      lsText                        : HandleText;
      lsPossiblyTagStart            : HandlePossiblyTagStart;
      lsTagStart                    : HandleTagStart;
      lsPossiblyTagEnd              : HandlePossiblyTagEnd;
      lsTagEnd                      : HandleTagEnd;
      lsEscapedVariable,
      lsUnescapedVariable           : HandleVariable;
      lsPossiblyUnescapedVariableEnd: HandlePossiblyUnescapedVariableEnd;
      lsUnescapedVariableAmp        : HandleUnescapedVariableAmp;
      lsOpenSection,lsCloseSection  : HandleSection;
    end;
  end;
  if Lexeme <> EmptyStr then begin
    AddToken(StartLine,StartCol,Kind,Lexeme,ContextBasePath,Result);
  end;
end;

function HTMLEncode(const Data: string): string;
var
  iPos, i: Integer;

  procedure Encode(const AStr: String);
  begin
    Move(AStr[1], result[iPos], Length(AStr) * SizeOf(Char));
    Inc(iPos, Length(AStr));
  end;

begin
  SetLength(result, Length(Data) * 6);
  iPos := 1;
  for i := 1 to length(Data) do
    case Data[i] of
      '<': Encode('&lt;');
      '>': Encode('&gt;');
      '&': Encode('&amp;');
      '"': Encode('&quot;');
    else
      result[iPos] := Data[i];
      Inc(iPos);
    end;
  SetLength(result, iPos - 1);
end;

procedure Interpret(const Tokens: TTokenList; const Context: TJSONObject; Result: TStream);
var
  Output: Text;
  Token: TToken;
  Value,BasePath: String;
  Node: TJSONData;
  p: SizeInt;
begin
  AssignStream(Output,Result);
  Rewrite(Output);
  for Token in Tokens do
    case Token.Kind of
      tkText: begin
        // WriteLn('Rendering "' + Escape(Token.Lexeme) + '"');
        Write(Output,Token.Lexeme);
      end;
      tkEscapedVariable,tkUnescapedVariable: begin
        Value := '';

        BasePath := Token.ContextBasePath;
        repeat
          Node := Context.FindPath(BasePath + Token.Lexeme);
          p := RPos('.',BasePath);
          if p > 0 then Delete(BasePath,p,Length(BasePath) - p);
        until Assigned(Node) or (BasePath = EmptyStr);

        if Assigned(Node) then
          case Node.JSONType of
            jtUnknown: raise Exception.Create('Don''t know how to interpret "' + Node.AsJSON + '"');
            jtNumber: begin
              if Node is TJSONFloatNumber then
                Value := FloatToStr(Node.AsFloat)
              else
                Value := Node.AsString;
            end;
            jtString: if Token.Kind = tkEscapedVariable then
              Value := HTMLEncode(Node.AsString)
            else
              Value := Node.AsString;
            jtBoolean: Value := BoolToStr(Node.AsBoolean);
            jtNull: ;
            jtArray: raise Exception.Create('Can''t interpolate array directly');
            jtObject: raise Exception.Create('Can''t interpolate object directly');
          end;
        Write(Output,Value);
      end;
    end;
end;

procedure Render(const Template: TStream; const Context: TJSONObject; Result: TStream);
var
  Tokens: TTokenList;
  Token: TToken;
begin
  if not Assigned(OnGetCache) or not OnGetCache(Template,Context,Result) then begin
    // initialize input and lexer
    AssignStream(Input,Template);
    Reset(Input);
    Line := 1;
    Col := 0;
    Tokens := Tokenize;
    Interpret(Tokens,Context,Result);
    if Assigned(OnPutCache) then OnPutCache(Template,Context,Result);
  end;
end;

end.
