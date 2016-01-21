
;RPAT Installer
;Ben Stabler, ben.stabler@rsginc.com, 03/19/15

[Setup]
AppId={{9DFC9A54-565E-4DC4-AC74-BE34247AFC58}
AppName=Rapid Policy Assessment Tool (RPAT)
AppVerName=RPAT
AppVersion=1.0
AppPublisher=RSG, Inc.
AppPublisherURL=http://www.rsginc.com
AppSupportURL=http://www.econ-works.org/10/travelworks.html
AppUpdatesURL=http://www.econ-works.org/10/travelworks.html
DefaultDirName={sd}\RPAT
DefaultGroupName=RPAT
AllowNoIcons=yes
OutputBaseFilename=RPAT_Installer
OutputDir="."
Compression=lzma
SolidCompression=yes
SetupIconFile=".\RPAT.ico"
UninstallDisplayIcon={uninstallexe}
SetupLogging=yes
InfoBeforeFile=ReadMe.txt

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Dirs]
Name: "{app}\projects"
Name: "{app}\scripts"
Name: "{app}\R"
Name: "{app}\gui"

[Files]
Source: "projects\*"; DestDir: "{app}\projects"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "scripts\*"; DestDir: "{app}\scripts"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "R\*"; DestDir: "{app}\R"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "gui\*"; DestDir: "{app}\gui"; Flags: ignoreversion recursesubdirs createallsubdirs

[Run]
Filename: "{app}\R\R-3.1.3-win.exe"; Parameters: "/SILENT /DIR=""{app}\R"""
Filename: "{app}\R\install_R_packages.bat"; Parameters: """{app}"""

[Icons]
Name: "{group}\RPAT"; Filename: "{app}\gui\RPAT.exe"; WorkingDir: "{app}\gui"
Name: "{group}\{cm:UninstallProgram,RPAT}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\RPAT"; Filename: "{app}\gui\RPAT.exe"; WorkingDir: "{app}\gui"; IconFilename: "{app}\gui\views\img\RPAT.ico";  Tasks: desktopicon

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}";

[UninstallRun]
Filename: "{app}\R\unins000.exe"

[UninstallDelete]
Type: filesandordirs; Name: "{app}\R"
Type: filesandordirs; Name: "{app}\scripts"
Type: filesandordirs; Name: "{app}\projects"
Type: filesandordirs; Name: "{app}\gui"
