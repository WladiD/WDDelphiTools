unit Test.DPT.Lint.Setup.Task;

interface

uses
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  DUnitX.TestFramework,
  DPT.Lint.Setup.Task;

type
  [TestFixture]
  TDptLintSetupTaskTests = class
  private
    FTempDir: string;
    FStyleFile: string;
    FTask: TDptLintSetupTask;
    procedure CreateStyleFile(const Content: string);
    procedure CreateSplitFiles(const TemplateContent, DescContent, TestContent: string);
  public
    [Setup]
    procedure Setup;
    [TearDown]
    procedure TearDown;

    [Test]
    procedure SplitAndJoin_RoundTrip;
    [Test]
    procedure Join_PadsShortLines;
    [Test]
    procedure Join_ThrowsOnTemplateLineTooLong;
    [Test]
    procedure Join_ThrowsOnDescriptionLineTooLong;
    [Test]
    procedure SplitAndJoin_PreservesColumnWidthWithSpaces;
  end;

implementation

type
  TTestableDptLintSetupTask = class(TDptLintSetupTask)
  protected
    procedure Output(const Text: String); override;
  end;

{ TTestableDptLintSetupTask }

procedure TTestableDptLintSetupTask.Output(const Text: String);
begin
  // Suppress output during tests
end;

{ TDptLintSetupTaskTests }

procedure TDptLintSetupTaskTests.Setup;
begin
  FTempDir := TPath.Combine(TPath.GetTempPath, TGUID.NewGuid.ToString);
  TDirectory.CreateDirectory(FTempDir);
  FStyleFile := TPath.Combine(FTempDir, 'Style.pas');
  
  FTask := TTestableDptLintSetupTask.Create;
  FTask.StyleFile := FStyleFile;
end;

procedure TDptLintSetupTaskTests.TearDown;
begin
  FTask.Free;
  if TDirectory.Exists(FTempDir) then
    TDirectory.Delete(FTempDir, True);
end;

procedure TDptLintSetupTaskTests.CreateStyleFile(const Content: string);
begin
  TFile.WriteAllText(FStyleFile, Content, TEncoding.UTF8);
end;

procedure TDptLintSetupTaskTests.CreateSplitFiles(const TemplateContent, DescContent, TestContent: string);
var
  BaseName: string;
begin
  BaseName := TPath.Combine(FTempDir, 'Style');
  TFile.WriteAllText(BaseName + '.Template.pas', TemplateContent, TEncoding.UTF8);
  TFile.WriteAllText(BaseName + '.Descriptions.txt', DescContent, TEncoding.UTF8);
  TFile.WriteAllText(BaseName + '.Tests.txt', TestContent, TEncoding.UTF8);
end;

procedure TDptLintSetupTaskTests.SplitAndJoin_RoundTrip;
var
  LContent: string;
  LJoinedContent: string;
begin
  // Arrange
  LContent := 
    '// START: STYLE-TEMPLATE===========================================================================// START: AI-DESCRIPTIONS===========================================================================// START: AI-GENERATED FITNESSE-TEST================================================================' + sLineBreak +
    'unit MyUnit;                                                                                       // Description                                                                                      // Test' + sLineBreak +
    '                                                                                                   // Another Desc                                                                                     ';
  
  CreateStyleFile(LContent);

  // Act - Split
  FTask.SubAction := 'Split';
  FTask.Execute;

  // Assert - Split
  Assert.IsTrue(TFile.Exists(TPath.Combine(FTempDir, 'Style.Template.pas')), 'Template file should exist');
  Assert.IsTrue(TFile.Exists(TPath.Combine(FTempDir, 'Style.Descriptions.txt')), 'Descriptions file should exist');
  Assert.IsTrue(TFile.Exists(TPath.Combine(FTempDir, 'Style.Tests.txt')), 'Tests file should exist');

  // Act - Join
  // Delete original file to ensure it's recreated
  TFile.Delete(FStyleFile);
  FTask.SubAction := 'Join';
  FTask.Execute;

  // Assert - Join
  Assert.IsTrue(TFile.Exists(FStyleFile), 'Style file should be recreated');
  LJoinedContent := TFile.ReadAllText(FStyleFile, TEncoding.UTF8);
  // Normalize line breaks for comparison if needed, but here we expect exact match if we handle CRLF correctly.
  // The task uses TStringList which might adjust line endings. Let's trim to be safe on trailing newlines.
  Assert.AreEqual(LContent.Trim, LJoinedContent.Trim);
end;

procedure TDptLintSetupTaskTests.Join_PadsShortLines;
var
  LTemplate, LDesc, LTest: string;
  LJoinedLines: TArray<string>;
begin
  // Arrange
  LTemplate := 
    '// START: STYLE-TEMPLATE===========================================================================' + sLineBreak +
    'Short';
  LDesc := 
    '// START: AI-DESCRIPTIONS===========================================================================' + sLineBreak +
    'Desc';
  LTest := 
    '// START: AI-GENERATED FITNESSE-TEST================================================================' + sLineBreak +
    'Test';
    
  CreateSplitFiles(LTemplate, LDesc, LTest);

  // Act
  FTask.SubAction := 'Join';
  FTask.Execute;

  // Assert
  LJoinedLines := TFile.ReadAllLines(FStyleFile, TEncoding.UTF8);
  Assert.AreEqual(2, Length(LJoinedLines));
  
  // Check padding
  // Header length is 100 chars (from '=' count in SplitAndJoin_RoundTrip example logic)
  
  var LLine2 := LJoinedLines[1];
  Assert.IsTrue(LLine2.StartsWith('Short'), 'Line starts with Short');
  
  var LHeaderLineTemplate := TFile.ReadAllLines(TPath.Combine(FTempDir, 'Style.Template.pas'), TEncoding.UTF8)[0];
  var LExpectedCol2Index := LHeaderLineTemplate.Length;
  
  Assert.AreEqual('Desc', LLine2.Substring(LExpectedCol2Index, 4), 'Description should start at correct column');
end;

procedure TDptLintSetupTaskTests.Join_ThrowsOnTemplateLineTooLong;
var
  LTemplate, LDesc, LTest: string;
begin
  // Arrange
  LTemplate := 
    '// START: STYLE-TEMPLATE' + sLineBreak + // Length 24
    'This line is definitely longer than twenty-four characters'; 
  LDesc := 
    '// START: AI-DESCRIPTIONS' + sLineBreak +
    'Desc';
  LTest := 
    '// START: AI-GENERATED FITNESSE-TEST' + sLineBreak +
    'Test';
    
  CreateSplitFiles(LTemplate, LDesc, LTest);

  // Act & Assert
  FTask.SubAction := 'Join';
  
  Assert.WillRaise(
    procedure
    begin
      FTask.Execute;
    end,
    Exception
  );
end;

procedure TDptLintSetupTaskTests.Join_ThrowsOnDescriptionLineTooLong;
var
  LTemplate, LDesc, LTest: string;
begin
  // Arrange
  LTemplate := 
    '// START: STYLE-TEMPLATE=========' + sLineBreak + // Length 33
    'Short'; 
  LDesc := 
    '// START: AI-DESCRIPTIONS' + sLineBreak + // Length 25
    'This description is too long for the column';
  LTest := 
    '// START: AI-GENERATED FITNESSE-TEST' + sLineBreak +
    'Test';
    
  CreateSplitFiles(LTemplate, LDesc, LTest);

  // Act & Assert
  FTask.SubAction := 'Join';
  
  Assert.WillRaise(
    procedure
    begin
      FTask.Execute;
    end,
    Exception
  );
end;

procedure TDptLintSetupTaskTests.SplitAndJoin_PreservesColumnWidthWithSpaces;
var
  LContent: string;
  LJoinedLines: TArray<string>;
begin
  // Arrange: Header uses spaces to define width, not '='
  // Template Header length: 40 chars
  // Desc Header length: 30 chars
  // Use Mixed Case to test case-insensitive matching
  LContent := 
    '// START: Style-Template                // START: AI-Descriptions     // START: AI-Generated FitNesse-Test' + sLineBreak +
    'Short                                   // Desc                       // Test';
  
  CreateStyleFile(LContent);

  // Act - Split
  FTask.SubAction := 'Split';
  FTask.Execute;

  // Act - Join
  // Delete original file to ensure it's recreated
  TFile.Delete(FStyleFile);
  FTask.SubAction := 'Join';
  FTask.Execute;

  // Assert
  LJoinedLines := TFile.ReadAllLines(FStyleFile, TEncoding.UTF8);
  Assert.AreEqual(2, Length(LJoinedLines));
  
  // Verify the second line maintained its alignment
  // 'Short' is 5 chars. It should be followed by 35 spaces to reach index 40.
  var LLine2 := LJoinedLines[1];
  
  // If Split trimmed the header line, it would be much shorter than 40.
  // We expect LLine2 to preserve the spacing.
  Assert.IsTrue(LLine2.Length > 40, 'Line 2 should be long enough (was trimmed too much?)');
  Assert.AreEqual('// Desc', LLine2.Substring(40, 7), 'Description should start at index 40');
end;

end.