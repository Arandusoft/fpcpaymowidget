{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit fpcpaymowidgetcontrols;

{$warn 5023 off : no warning about unused units}
interface

uses
  AnimatedPanel, AutoCompletePanel, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('AnimatedPanel', @AnimatedPanel.Register);
  RegisterUnit('AutoCompletePanel', @AutoCompletePanel.Register);
end;

initialization
  RegisterPackage('fpcpaymowidgetcontrols', @Register);
end.
