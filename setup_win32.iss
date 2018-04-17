; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "FPC Paymo Widget"
#define MyAppVersion "1.0.8"
#define MyAppPublisher "Arand� Software"
#define MyAppURL "http://www.arandusoft.com/"
#define MyAppExeName "paymowidget.exe"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{4BC11937-32E8-47DB-9AD6-87DEA59583DF}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
DefaultDirName={pf}\{#MyAppName}
DisableDirPage=yes
DisableProgramGroupPage=yes
OutputDir=setup
OutputBaseFilename=fpcpaymowidget_{#MyAppVersion}
Compression=lzma
SolidCompression=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "spanish"; MessagesFile: "compiler:Languages\Spanish.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: ".\bin\i386-win32\paymowidget.exe"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: ".\dll\libeay32.dll"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: ".\dll\ssleay32.dll"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: ".\i18n\*"; DestDir: "{app}\locale\"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: ".\fonts\Nunito_Sans\NunitoSans-Black.ttf"; DestDir: "{fonts}"; FontInstall: "Nunito Sans Black"; Flags: onlyifdoesntexist uninsneveruninstall
Source: ".\fonts\Nunito_Sans\NunitoSans-BlackItalic.ttf"; DestDir: "{fonts}"; FontInstall: "Nunito Sans Black Italic"; Flags: onlyifdoesntexist uninsneveruninstall
Source: ".\fonts\Nunito_Sans\NunitoSans-Bold.ttf"; DestDir: "{fonts}"; FontInstall: "Nunito Sans Bold"; Flags: onlyifdoesntexist uninsneveruninstall
Source: ".\fonts\Nunito_Sans\NunitoSans-BoldItalic.ttf"; DestDir: "{fonts}"; FontInstall: "Nunito Sans Bold Italic"; Flags: onlyifdoesntexist uninsneveruninstall
Source: ".\fonts\Nunito_Sans\NunitoSans-ExtraBold.ttf"; DestDir: "{fonts}"; FontInstall: "Nunito Sans ExtraBold"; Flags: onlyifdoesntexist uninsneveruninstall
Source: ".\fonts\Nunito_Sans\NunitoSans-ExtraBoldItalic.ttf"; DestDir: "{fonts}"; FontInstall: "Nunito Sans ExtraBold Italic"; Flags: onlyifdoesntexist uninsneveruninstall
Source: ".\fonts\Nunito_Sans\NunitoSans-ExtraLight.ttf"; DestDir: "{fonts}"; FontInstall: "Nunito Sans ExtraLight"; Flags: onlyifdoesntexist uninsneveruninstall
Source: ".\fonts\Nunito_Sans\NunitoSans-ExtraLightItalic.ttf"; DestDir: "{fonts}"; FontInstall: "Nunito Sans ExtraLight Italic"; Flags: onlyifdoesntexist uninsneveruninstall
Source: ".\fonts\Nunito_Sans\NunitoSans-Italic.ttf"; DestDir: "{fonts}"; FontInstall: "Nunito Sans Italic"; Flags: onlyifdoesntexist uninsneveruninstall
Source: ".\fonts\Nunito_Sans\NunitoSans-Light.ttf"; DestDir: "{fonts}"; FontInstall: "Nunito Sans Light"; Flags: onlyifdoesntexist uninsneveruninstall
Source: ".\fonts\Nunito_Sans\NunitoSans-LightItalic.ttf"; DestDir: "{fonts}"; FontInstall: "Nunito Sans Light Italic"; Flags: onlyifdoesntexist uninsneveruninstall
Source: ".\fonts\Nunito_Sans\NunitoSans-Regular.ttf"; DestDir: "{fonts}"; FontInstall: "Nunito Sans Regular"; Flags: onlyifdoesntexist uninsneveruninstall
Source: ".\fonts\Nunito_Sans\NunitoSans-SemiBold.ttf"; DestDir: "{fonts}"; FontInstall: "Nunito Sans SemiBold"; Flags: onlyifdoesntexist uninsneveruninstall
Source: ".\fonts\Nunito_Sans\NunitoSans-SemiBoldItalic.ttf"; DestDir: "{fonts}"; FontInstall: "Nunito Sans SemiBold Italic"; Flags: onlyifdoesntexist uninsneveruninstall

[Icons]
Name: "{commonprograms}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{commondesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

