@rem ***********************************************************************
@rem *                                                                     *
@rem *                                 OCaml                               *
@rem *                                                                     *
@rem *                 David Allsopp, OCaml Labs, Cambridge.               *
@rem *                                                                     *
@rem *   Copyright 2017 MetaStack Solutions Ltd.                           *
@rem *                                                                     *
@rem *   All rights reserved.  This file is distributed under the terms of *
@rem *   the GNU Lesser General Public License version 2.1, with the       *
@rem *   special exception on linking described in the file LICENSE.       *
@rem *                                                                     *
@rem ***********************************************************************

@rem BE CAREFUL ALTERING THIS FILE TO ENSURE THAT ERRORS PROPAGATE
@rem IF A COMMAND SHOULD FAIL IT PROBABLY NEEDS TO END WITH
@rem   || exit /b 1
@rem BASICALLY, DO THE TESTING IN BASH...

@rem Do not call setlocal!
@echo off

goto %1

goto :EOF

:SaveVars
set OCAML_PREV_PATH=%PATH%
set OCAML_PREV_LIB=%LIB%
set OCAML_PREV_INCLUDE=%INCLUDE%
goto :EOF

:RestoreVars
set PATH=%OCAML_PREV_PATH%
set LIB=%OCAML_PREV_LIB%
set INCLUDE=%OCAML_PREV_INCLUDE%
goto :EOF

:CheckPackage
"%CYG_ROOT%\bin\bash.exe" -lc "cygcheck -dc %1" | findstr %1 > nul
if %ERRORLEVEL% equ 1 (
  echo Cygwin package %1 will be installed
  set CYGWIN_INSTALL_PACKAGES=%CYGWIN_INSTALL_PACKAGES%,%1
)
goto :EOF

:UpgradeCygwin
if "%CYGWIN_INSTALL_PACKAGES%" neq "" "%CYG_ROOT%\setup-x86_64.exe" --quiet-mode --no-shortcuts --no-startmenu --no-desktop --only-site --root "%CYG_ROOT%" --site "%CYG_MIRROR%" --local-package-dir "%CYG_CACHE%" --packages %CYGWIN_INSTALL_PACKAGES:~1% > nul
for %%P in (%CYGWIN_COMMANDS%) do "%CYG_ROOT%\bin\%%P.exe" --version > nul || set CYGWIN_UPGRADE_REQUIRED=1
"%CYG_ROOT%\bin\bash.exe" -lc "cygcheck -dc %CYGWIN_PACKAGES%"
if %CYGWIN_UPGRADE_REQUIRED% equ 1 (
  echo Cygwin package upgrade required - please go and drink coffee
  "%CYG_ROOT%\setup-x86_64.exe" --quiet-mode --no-shortcuts --no-startmenu --no-desktop --only-site --root "%CYG_ROOT%" --site "%CYG_MIRROR%" --local-package-dir "%CYG_CACHE%" --upgrade-also > nul
  "%CYG_ROOT%\bin\bash.exe" -lc "cygcheck -dc %CYGWIN_PACKAGES%"
)
goto :EOF

:install
chcp 65001 > nul
rem This must be kept in sync with appveyor_build.sh
set BUILD_PREFIX=🐫реализация
git worktree add "..\%BUILD_PREFIX%-msvc64" -b appveyor-build-msvc64
git worktree add "..\%BUILD_PREFIX%-mingw32" -b appveyor-build-mingw32
git worktree add "..\%BUILD_PREFIX%-msvc32" -b appveyor-build-msvc32
cd "..\%BUILD_PREFIX%-mingw32"
git submodule update --init flexdll

cd "%APPVEYOR_BUILD_FOLDER%"
appveyor DownloadFile "https://github.com/alainfrisch/flexdll/archive/0.37.tar.gz" -FileName "flexdll.tar.gz" || exit /b 1
appveyor DownloadFile "https://github.com/alainfrisch/flexdll/releases/download/0.37/flexdll-bin-0.37.zip" -FileName "flexdll.zip" || exit /b 1
rem flexdll.zip is processed here, rather than in appveyor_build.sh because the
rem unzip command comes from MSYS2 (via Git for Windows) and it has to be
rem invoked via cmd /c in a bash script which is weird(er).
mkdir "%APPVEYOR_BUILD_FOLDER%\..\flexdll"
move flexdll.zip "%APPVEYOR_BUILD_FOLDER%\..\flexdll"
cd "%APPVEYOR_BUILD_FOLDER%\..\flexdll" && unzip -q flexdll.zip

rem CYGWIN_PACKAGES is the list of required Cygwin packages (cygwin is included
rem in the list just so that the Cygwin version is always displayed on the log).
rem CYGWIN_COMMANDS is a corresponding command to run with --version to test
rem whether the package works. This is used to verify whether the installation
rem needs upgrading.
set CYGWIN_PACKAGES=cygwin make diffutils mingw64-i686-gcc-core
set CYGWIN_COMMANDS=cygcheck make diff i686-w64-mingw32-gcc

set CYGWIN_INSTALL_PACKAGES=
set CYGWIN_UPGRADE_REQUIRED=0

for %%P in (%CYGWIN_PACKAGES%) do call :CheckPackage %%P
call :UpgradeCygwin

"%CYG_ROOT%\bin\bash.exe" -lec "$APPVEYOR_BUILD_FOLDER/appveyor_build.sh install" || exit /b 1

call :SaveVars
goto :EOF

:build
rem Run the msvc64 and mingw32 builds
call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\amd64\vcvars64.bat"
"%CYG_ROOT%\bin\bash.exe" -lec "$APPVEYOR_BUILD_FOLDER/appveyor_build.sh" || exit /b 1

rem Reconfigure the environment and run the msvc32 partial build
call :RestoreVars
call "C:\Program Files\Microsoft SDKs\Windows\v7.1\Bin\SetEnv.cmd" /x86
"%CYG_ROOT%\bin\bash.exe" -lec "$APPVEYOR_BUILD_FOLDER/appveyor_build.sh msvc32-only" || exit /b 1
goto :EOF

:test
rem Reconfigure the environment for the msvc64 build
call :RestoreVars
call "C:\Program Files (x86)\Microsoft Visual Studio 14.0\VC\bin\amd64\vcvars64.bat"
"%CYG_ROOT%\bin\bash.exe" -lec "$APPVEYOR_BUILD_FOLDER/appveyor_build.sh test" || exit /b 1
goto :EOF
