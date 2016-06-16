// fpc -Fu/mnt/LinuxShared/lazarus-svn/components/lazutils/lib/x86_64-linux spectester.pas

program spectester;

{$mode objfpc}{$H+}

uses
  Classes,SysUtils,FileUtil,fpjson,jsonparser,mustapas;

var
  JSONFileContent: TStringList;
  Template,Expected,Result: String;
  JSON,Test,Context: TJSONObject;
  e: TJSONEnum;
  i: Integer;
  Verdict: Boolean;
  TemplateStream,ResultStream: TStringStream;
begin
  with FindAllFiles('specs','*.json') do
    try
      for i := 0 to Count - 1 do begin
        WriteLn('[' + Strings[i] + ']');
        JSONFileContent := TStringList.Create;
        JSONFileContent.LoadFromFile(Strings[i]);
        JSON := GetJSON(JSONFileContent.Text) as TJSONObject;
        for e in JSON['tests'] do begin
          Test := e.Value as TJSONObject;
          Template := Test['template'].AsString;
          Context := Test['data'] as TJSONObject;
          Expected := Test['expected'].AsString;
          try
            TemplateStream := TStringStream.Create(Template);
            ResultStream := TStringStream.Create('');
              Mustapas.Render(TemplateStream,Context,ResultStream);
              Result := ResultStream.DataString;
            TemplateStream.Free;
            ResultStream.Free;

            Verdict := Result = Expected;
            WriteLn(Test['name'].AsString + ': ',Verdict);
            if not Verdict then begin
              WriteLn('Expected: ' + Escape(Expected));
              WriteLn('Actual  : ' + Escape(Result));
              WriteLn;
            end;
          except
            on e: Exception do begin
              WriteLn(e.ClassName + ': ' + e.Message)
            end;
          end;
        end;
      end;
    finally
      Free;
    end;
end.
