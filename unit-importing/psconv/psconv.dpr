program psconv;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.RegularExpressions,
  IdHashMessageDigest,
  ParserU;

function md5(iString: string): string;
var
  idmd5: TIdHashMessageDigest5;
begin
  with TIdHashMessageDigest5.Create do try
    Result := HashStringAsHex(iString)
  finally
    Free;
  end;
end;

var
  sl: TStringList;
  i: integer;
  outputFilename: string;
  killLine: boolean;

begin
  try
    if ParamCount <> 2 then begin
      WriteLn('Usage: psconv.exe <source file> <output path>');
      exit;
    end;

    sl := TStringList.Create;
    try
      WriteLn('input: ' + ParamStr(1));

      // Load source file
      sl.LoadFromFile(ParamStr(1));

      // Replace methods that are marked with "// ScriptFilterMethod".
      // We use a name derived from the MD5 hash of the entire line to
      // ensure that the method name is consistent in the output file.
      for i := 0 to sl.Count - 1 do
        if sl[i].Contains('// ScriptFilterMethod') then
          sl[i] := 'procedure FilteredMethod_' + md5(sl[i]) + ';';

      // Remove lines that have "NoScripting" in them
      i := 0;
      killLine := false;
      while i < sl.Count do
        if sl[i].Contains('NoScripting') then begin
          if sl[i].Contains('NoScriptingBegin') then
            killLine := true;
          if sl[i].Contains('NoScriptingEnd') then
            killLine := false;
          sl.Delete(i);
        end
        else if killLine then
          sl.Delete(i)
        else
          inc(i);

      // Remove lines that look like [Attributes]
      i := 0;
      while i < sl.Count do
        if TRegEx.IsMatch(sl[i], '^\s*\[[A-Za-z]') then
          sl.Delete(i)
        else
          inc(i);

      // Create parser
      with TUnitParser.Create('conv.ini') do try

        // some useful settings
        AutoRenameOverloadedMethods := true;

        // parse
        ParseUnit(sl.Text);

        // copy output into sl
        sl.Text := OutUnitList.Text;

        outputFilename := IncludeTrailingPathDelimiter(ParamStr(2)) + UnitNameCmp;
        System.Writeln('output: ' + outputFilename);
        sl.SaveToFile(outputFilename);
      finally
        Free;
      end;


    finally
      sl.Free;
    end;

    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
