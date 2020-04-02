mkdir "%APPDATA%\TerminusDB"
mkdir "%APPDATA%\TerminusDB\storage"
mkdir "%APPDATA%\TerminusDB\tmp"
set TERMINUS_SERVER_DB_PATH=%APPDATA%\TerminusDB\storage
set TERMINUS_SERVER_TMP_PATH=%APPDATA%\TerminusDB\tmp
set TERMINUS_SERVER_INDEX_PATH=%APPDATA%\TerminusDB\index.html
set TERMINUS_LOG_PATH=%APPDATA%\TerminusDB\httpd.log
if not exist "%TERMINUS_SERVER_DB_PATH%\db" swipl utils\db_util -s localhost -k root --autologin=true
swipl start.pl
