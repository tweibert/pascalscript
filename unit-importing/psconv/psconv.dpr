program psconv;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  System.RegularExpressions,
  ParserU;

var
  sl: TStringList;
  i: integer;
  outputFilename: string;

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

      // Remove lines that have "NoScripting" in them
      i := 0;
      while i < sl.Count do
        if pos('NoScripting', sl[i]) > 0 then
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
