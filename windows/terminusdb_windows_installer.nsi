;-------------------------------------------------------------------------------
; Includes
!include "MUI2.nsh"
!include "LogicLib.nsh"
!include "WinVer.nsh"
!include "x64.nsh"

;-------------------------------------------------------------------------------
; Constants
!define PRODUCT_NAME "TerminusDB"
!define PRODUCT_DESCRIPTION "The best database"
!define COPYRIGHT "Copyright © 2020 DataChemist LTD"
!define PRODUCT_VERSION "1.0.0.0"
!define SETUP_VERSION 1.0.0.0
!define MUI_WELCOMEPAGE_TEXT "Install TerminusDB to make your life better!"
!define MUI_FINISHPAGE_TEXT "Thank you for installing TerminusDB."
;-------------------------------------------------------------------------------
; Attributes
Name "TerminusDB"
OutFile "terminusdb_setup.exe"
InstallDir "$PROGRAMFILES64\terminus_server"
;InstallDirRegKey HKCU "Software\DataChemist\terminus_server" ""
RequestExecutionLevel admin ; user|highest|admin

;-------------------------------------------------------------------------------
; Installer Pages
;!insertmacro MUI_PAGE_WELCOME
;!insertmacro MUI_PAGE_LICENSE "${NSISDIR}\Docs\Modern UI\License.txt"
!insertmacro MUI_PAGE_DIRECTORY
!insertmacro MUI_PAGE_INSTFILES
!insertmacro MUI_PAGE_FINISH

;-------------------------------------------------------------------------------
; Uninstaller Pages
!insertmacro MUI_UNPAGE_WELCOME
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
!insertmacro MUI_UNPAGE_FINISH


;-------------------------------------------------------------------------------
; Installer Sections
Section "TerminusDB" TerminusDB
	SetOutPath $INSTDIR
	;File "My Program.exe"
	;File "Readme.txt"
	File "terminus_store_prolog.dll"
	File "libterminus_store.dll"
	File /r "terminus-server\*"
    SetOutPath $APPDATA\SWI-Prolog\pack\terminus_store_prolog
    File /r "terminus_store_prolog\*"
SectionEnd

;-------------------------------------------------------------------------------
; Uninstaller Sections
Section "Uninstall"
	RMDir "$INSTDIR"
	DeleteRegKey /ifempty HKCU "Software\DataChemist\terminus_server"
SectionEnd
