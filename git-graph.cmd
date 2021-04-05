@echo off
set GITCMD=%USERPROFILE%\AppData\Local\GitHubDesktop\app-2.7.1\resources\app\git\cmd
%GITCMD%\Git log --all --decorate --oneline --graph --date=short --pretty="%%C(white)%%s %%C(green)[%%ad] %%C(yellow)%%d"
pause