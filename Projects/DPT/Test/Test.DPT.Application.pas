// ======================================================================
// Copyright (c) 2026 Waldemar Derr. All rights reserved.
//
// Licensed under the MIT license. See included LICENSE file for details.
// ======================================================================

unit Test.DPT.Application;

interface

uses

  System.SysUtils,

  DUnitX.TestFramework,

  DPT.Application;

type

  /// <summary>
  ///   Exercises <see cref="TDptApplication.IsSlimServerStart"/>: the decision
  ///   whether a command line starts the embedded Slim server or a CLI action.
  /// </summary>
  [TestFixture]
  TTestSlimServerStart = class
  public
    // ---- Slim server requested ----
    [Test]
    procedure SlimPortSwitch_Alone_StartsServer;
    [Test]
    procedure SlimPortSwitch_AmongOtherParams_StartsServer;
    [Test]
    procedure SlimPortSwitch_IsCaseInsensitive;
    [Test]
    procedure SoleInteger_StartsServer;

    // ---- CLI action requested ----
    [Test]
    procedure NoParams_NoServer;
    [Test]
    procedure SoleNonInteger_NoServer;
    [Test]
    procedure SlimPortSwitch_WithoutNumber_NoServer;
    [Test]
    procedure BuildAndRun_WithRunArgsAfterSeparator_NoServer;
    [Test]
    procedure OpenUnit_GoToLine_NoServer;
    [Test]
    procedure VersionAlias_FollowedByInteger_NoServer;
  end;

implementation

{ TTestSlimServerStart }

procedure TTestSlimServerStart.SlimPortSwitch_Alone_StartsServer;
var
  LPort: Integer;
begin
  Assert.IsTrue(TDptApplication.IsSlimServerStart(['--SlimPort=8085'], LPort));
  Assert.AreEqual(8085, LPort);
end;

procedure TTestSlimServerStart.SlimPortSwitch_AmongOtherParams_StartsServer;
var
  LPort: Integer;
begin
  Assert.IsTrue(TDptApplication.IsSlimServerStart(['--verbose', '--SlimPort=8085', 'x'], LPort));
  Assert.AreEqual(8085, LPort);
end;

procedure TTestSlimServerStart.SlimPortSwitch_IsCaseInsensitive;
var
  LPort: Integer;
begin
  Assert.IsTrue(TDptApplication.IsSlimServerStart(['--slimport=9001'], LPort));
  Assert.AreEqual(9001, LPort);
end;

procedure TTestSlimServerStart.SoleInteger_StartsServer;
var
  LPort: Integer;
begin
  // FitNesse convention: COMMAND_PATTERN + port, so the port is the only arg
  Assert.IsTrue(TDptApplication.IsSlimServerStart(['9066'], LPort));
  Assert.AreEqual(9066, LPort);
end;

procedure TTestSlimServerStart.NoParams_NoServer;
var
  LPort: Integer;
begin
  Assert.IsFalse(TDptApplication.IsSlimServerStart([], LPort));
  Assert.AreEqual(0, LPort);
end;

procedure TTestSlimServerStart.SoleNonInteger_NoServer;
var
  LPort: Integer;
begin
  Assert.IsFalse(TDptApplication.IsSlimServerStart(['Help'], LPort));
  Assert.AreEqual(0, LPort);
end;

procedure TTestSlimServerStart.SlimPortSwitch_WithoutNumber_NoServer;
var
  LPort: Integer;
begin
  Assert.IsFalse(TDptApplication.IsSlimServerStart(['--SlimPort=abc'], LPort));
  Assert.AreEqual(0, LPort);
end;

procedure TTestSlimServerStart.BuildAndRun_WithRunArgsAfterSeparator_NoServer;
var
  LPort: Integer;
begin
  // The reported regression: "-- 9066" are run arguments for the built exe,
  // but the trailing integer used to be taken as a Slim port and DPT started
  // its Slim server instead of running the build result.
  Assert.IsFalse(TDptApplication.IsSlimServerStart(
    ['Latest', 'BuildAndRun', 'TOOLS\SlimProxy\PAS\SlimProxy.dproj', 'Win32', 'Debug',
     '--OnlyIfChanged', '--', '9066'], LPort));
  Assert.AreEqual(0, LPort);
end;

procedure TTestSlimServerStart.OpenUnit_GoToLine_NoServer;
var
  LPort: Integer;
begin
  // Same shape without a separator: the number is the action's own argument
  Assert.IsFalse(TDptApplication.IsSlimServerStart(
    ['LATEST', 'OpenUnit', 'C:\Projects\MyUnit.pas', 'GoToLine', '42'], LPort));
  Assert.AreEqual(0, LPort);
end;

procedure TTestSlimServerStart.VersionAlias_FollowedByInteger_NoServer;
var
  LPort: Integer;
begin
  Assert.IsFalse(TDptApplication.IsSlimServerStart(['LATEST', '9000'], LPort));
  Assert.AreEqual(0, LPort);
end;

end.
