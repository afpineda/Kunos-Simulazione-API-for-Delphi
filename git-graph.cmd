@echo off
Git log --all --decorate --oneline --graph --date=short --pretty="%%C(white)%%s %%C(green)[%%ad] %%C(yellow)%%d"
pause