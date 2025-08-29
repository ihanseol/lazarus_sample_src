@ECHO OFF

REM 
REM * Batch file to download foreign exchange courses for "Devises" application using wget.exe
REM 
REM * Course list (with values corresponding to 1€) downloaded from https://www.banque-france.fr
REM * Program and error output directed to STDOUT (and cought by Devises.exe); website (HTML,
REM * containing the course values) saved into file devises.tmp (and extracted by Devises.exe)
REM

wget https://www.banque-france.fr/statistiques/taux-et-cours/les-taux-de-change-salle-des-marches/parites-quotidiennes 2>&1 -O devises.tmp 2>&1

IF %ERRORLEVEL%==8 GOTO Error8
IF %ERRORLEVEL%==7 GOTO Error7
IF %ERRORLEVEL%==6 GOTO Error6
IF %ERRORLEVEL%==5 GOTO Error5
IF %ERRORLEVEL%==4 GOTO Error4
IF %ERRORLEVEL%==3 GOTO Error3
IF %ERRORLEVEL%==2 GOTO Error2
IF %ERRORLEVEL%==1 GOTO Error1

REM * wget terminated with errorlevel 0 (ok)
ECHO Currency file successfully downloaded
GOTO End

REM * wget terminated with errorlevels 1-8 (problem)
:Error8
ECHO WGET error %ERRORLEVEL%: Server issued an error response
GOTO End
:Error7
ECHO WGET error %ERRORLEVEL%: Protocol error
GOTO End
:Error6
ECHO WGET error %ERRORLEVEL%: Username/password authentication failure
GOTO End
:Error5
ECHO WGET error %ERRORLEVEL%: SSL verification failure
GOTO End
:Error4
ECHO WGET error %ERRORLEVEL%: Network failure
GOTO End
:Error3
ECHO WGET error %ERRORLEVEL%: File I/O error
GOTO End
:Error2
ECHO WGET error %ERRORLEVEL%: Parse error
GOTO End
:Error1
ECHO WGET error %ERRORLEVEL%: Generic error
GOTO End

REM * End of program
:End
