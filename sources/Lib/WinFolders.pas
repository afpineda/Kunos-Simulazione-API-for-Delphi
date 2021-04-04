unit WinFolders;

interface

uses
  Winapi.Windows;

function getFolder(CSIDL: integer; Owner: HWND = 0; Token: THandle = 0): string;

type
  TWindowsFolder = class
  public
    class function appData: string;
    class function recycleBin: string;
    class function commonAdminTools: string;
    class function commonAppData: string;
    class function commonDesktopDir: string;
    class function commonDocuments: string;
    class function commonFavorites: string;
    class function commonMusic: string;
    class function commonPictures: string;
    class function commonStartMenuPrograms: string;
    class function commonStartMenu: string;
    class function commonStartup: string;
    class function commonTemplates: string;
    class function commonVideo: string;
    class function computersNearMe: string;
    class function connections: string;
    class function controlPanel: string;
    class function desktop: string;
    class function desktopDir: string;
    class function myComputer: string;
    class function favorites: string;
    class function fonts: string;
    class function internetHistory: string;
    class function internetCache: string;
    class function cookies: string;
    class function localAppData: string;
    class function myDocuments: string;
    class function myMusic: string;
    class function myPictures: string;
    class function myVideo: string;
    class function myNetworkPlaces: string;
    class function networkNeightborhood: string;
    class function photoAlbums: string;
    class function playlists: string;
    class function printers: string;
    class function userProfile: string;
    class function programFiles: string;
    class function programFilesX86: string;
    class function programFilesCommonDir: string;
    class function programFilesCommonDirX86: string;
    class function startMenuPrograms: string;
    class function recent: string;
    class function sendTo: string;
    class function startMenu: string;
    class function startup: string;
    class function system: string;
    class function systemX86: string;
    class function Windows: string;
  end;

implementation

uses
  SysUtils,
  Winapi.ShlObj;

function getFolder(CSIDL: integer; Owner: HWND; Token: THandle): string;
var
  pidl: PItemIDList;
  aux: array [1 .. MAX_PATH] of Char;
  shresult: integer;
begin
  shresult := SHGetFolderLocation(Owner, CSIDL, Token, 0, pidl);
  if (shresult = S_OK) then
  begin
    FillChar(aux, SizeOf(aux), 0);
    SHGetPathFromIDList(pidl, PChar(@aux));
    SetString(Result, PChar(@aux), Length(aux));
    Result := Trim(Result);
    ILFree(pidl);
  end
  else if (shresult = S_FALSE) then
    Result := ''
  else
    raise EArgumentException.Create('invalid CSIDL');
end;

class function TWindowsFolder.appData: string;
begin
  Result := getFolder(CSIDL_APPDATA);
end;

class function TWindowsFolder.recycleBin: string;
begin
  Result := getFolder(CSIDL_BITBUCKET);
end;

class function TWindowsFolder.commonAdminTools: string;
begin
  Result := getFolder(CSIDL_COMMON_ADMINTOOLS);
end;

class function TWindowsFolder.commonAppData: string;
begin
  Result := getFolder(CSIDL_COMMON_APPDATA);
end;

class function TWindowsFolder.commonDesktopDir: string;
begin
  Result := getFolder(CSIDL_COMMON_DESKTOPDIRECTORY);
end;

class function TWindowsFolder.commonDocuments: string;
begin
  Result := getFolder(CSIDL_COMMON_DOCUMENTS);
end;

class function TWindowsFolder.commonFavorites: string;
begin
  Result := getFolder(CSIDL_COMMON_FAVORITES);
end;

class function TWindowsFolder.commonMusic: string;
begin
  Result := getFolder(CSIDL_COMMON_MUSIC);
end;

class function TWindowsFolder.commonPictures: string;
begin
  Result := getFolder(CSIDL_COMMON_PICTURES);
end;

class function TWindowsFolder.commonStartMenuPrograms: string;
begin
  Result := getFolder(CSIDL_COMMON_PROGRAMS);
end;

class function TWindowsFolder.commonTemplates: string;
begin
  Result := getFolder(CSIDL_COMMON_TEMPLATES);
end;

class function TWindowsFolder.commonVideo: string;
begin
  Result := getFolder(CSIDL_COMMON_VIDEO);
end;

class function TWindowsFolder.computersNearMe: string;
begin
  Result := getFolder(CSIDL_COMPUTERSNEARME);
end;

class function TWindowsFolder.connections: string;
begin
  Result := getFolder(CSIDL_CONNECTIONS);
end;

class function TWindowsFolder.controlPanel: string;
begin
  Result := getFolder(CSIDL_CONTROLS);
end;

class function TWindowsFolder.desktop: string;
begin
  Result := getFolder(CSIDL_DESKTOP);
end;

class function TWindowsFolder.desktopDir: string;
begin
  Result := getFolder(CSIDL_DESKTOPDIRECTORY);
end;

class function TWindowsFolder.myComputer: string;
begin
  Result := getFolder(CSIDL_DRIVES);
end;

class function TWindowsFolder.favorites: string;
begin
  Result := getFolder(CSIDL_FAVORITES);
end;

class function TWindowsFolder.fonts: string;
begin
  Result := getFolder(CSIDL_FONTS);
end;

class function TWindowsFolder.internetHistory: string;
begin
  Result := getFolder(CSIDL_HISTORY);
end;

class function TWindowsFolder.internetCache: string;
begin
  Result := getFolder(CSIDL_INTERNET_CACHE);
end;

class function TWindowsFolder.localAppData: string;
begin
  Result := getFolder(CSIDL_LOCAL_APPDATA);
end;

class function TWindowsFolder.myDocuments: string;
begin
  Result := getFolder(CSIDL_MYDOCUMENTS);
end;

class function TWindowsFolder.myMusic: string;
begin
  Result := getFolder(CSIDL_MYMUSIC);
end;

class function TWindowsFolder.myPictures: string;
begin
  Result := getFolder(CSIDL_MYPICTURES);
end;

class function TWindowsFolder.myVideo: string;
begin
  Result := getFolder(CSIDL_MYVIDEO);
end;

class function TWindowsFolder.myNetworkPlaces: string;
begin
  Result := getFolder(CSIDL_NETHOOD);
end;

class function TWindowsFolder.networkNeightborhood: string;
begin
  Result := getFolder(CSIDL_NETWORK);
end;

class function TWindowsFolder.photoAlbums: string;
begin
  Result := getFolder($0045);
end;

class function TWindowsFolder.playlists: string;
begin
  Result := getFolder($003F);
end;

class function TWindowsFolder.printers: string;
begin
  Result := getFolder(CSIDL_PRINTERS);
end;

class function TWindowsFolder.userProfile: string;
begin
  Result := getFolder(CSIDL_PROFILE);
end;

class function TWindowsFolder.programFiles: string;
begin
  Result := getFolder(CSIDL_PROGRAM_FILES);
end;

class function TWindowsFolder.programFilesCommonDir: string;
begin
  Result := getFolder(CSIDL_PROGRAM_FILES_COMMON);
end;

class function TWindowsFolder.startMenuPrograms: string;
begin
  Result := getFolder(CSIDL_PROGRAMS);
end;

class function TWindowsFolder.recent: string;
begin
  Result := getFolder(CSIDL_RECENT);
end;

class function TWindowsFolder.sendTo: string;
begin
  Result := getFolder(CSIDL_SENDTO);
end;

class function TWindowsFolder.startMenu: string;
begin
  Result := getFolder(CSIDL_STARTMENU);
end;

class function TWindowsFolder.startup: string;
begin
  Result := getFolder(CSIDL_STARTUP);
end;

class function TWindowsFolder.system: string;
begin
  Result := getFolder(CSIDL_SYSTEM);
end;

class function TWindowsFolder.Windows: string;
begin
  Result := getFolder(CSIDL_WINDOWS);
end;

class function TWindowsFolder.commonStartMenu: string;
begin
  Result := getFolder(CSIDL_COMMON_STARTMENU);
end;

class function TWindowsFolder.commonStartup: string;
begin
  Result := getFolder(CSIDL_COMMON_STARTMENU);
end;

class function TWindowsFolder.cookies: string;
begin
  Result := getFolder(CSIDL_COOKIES);
end;

class function TWindowsFolder.systemX86: string;
begin
  Result := getFolder(CSIDL_SYSTEMX86);
end;

class function TWindowsFolder.programFilesX86: string;
begin
  Result := getFolder(CSIDL_PROGRAM_FILESX86);
end;

class function TWindowsFolder.programFilesCommonDirX86: string;
begin
  Result := getFolder(CSIDL_PROGRAM_FILES_COMMONX86);
end;

end.
