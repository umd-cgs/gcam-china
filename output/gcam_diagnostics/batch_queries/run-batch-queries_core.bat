@echo off

REM Find the various Model Interface components and dependencies where they live
REM in the release package.
SET CLASSPATH=..\..\..\libs\jars\*;..\..\..\output\modelinterface\ModelInterface.jar

REM Run the Model Interface.  Note we are redirecting output to a log file with the
REM -l option.  Users could also modify the command below to use the -b option to
REM a Model Interface Batch Command file.
java  ModelInterface.InterfaceMain -b ..\batch_queries\test.xml  -l .\run_batch_queries.txt
pause


