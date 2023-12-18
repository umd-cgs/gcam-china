@echo off

REM Users may set the following location to the appropriate Java Runtime installation location
REM instead of trying to detect the appropriate location.  This may be necessary if the default
REM Java version is the 32-bit runtime.
REM SET JAVA_HOME=C:\Program Files\Java\jre1.8.0_101
REM Update the PATH to be able to find the Java dlls
REM SET PATH=%JAVA_HOME%\bin;%JAVA_HOME%\bin\server

REM Find the various Model Interface components and dependencies where they live
REM in the release package.
SET CLASSPATH=..\libs\jars\*;..\output\modelinterface\ModelInterface.jar

REM Run the Model Interface.  Note we are redirecting output to a log file with the
REM -l option.  Users could also modify the command below to use the -b option to
REM a Model Interface Batch Command file.
java ModelInterface.InterfaceMain -l logs/model_interface_log.txt
