@echo off

REM Attempt to find the JAVA_HOME so that we can update the PATH variable to find the JVM dlls
REM before loading GCAM.

REM Users may set the following location to the appropriate Java Runtime installation location
REM instead of trying to detect the appropriate location.  This may be necessary if the default
REM Java version is the 32-bit runtime.
REM SET JAVA_HOME=C:\Program Files\Java\jre1.8.0_101


REM Attempt to use XMLDBDriver which will print the java.home property of the Java Runtime
REM used to run it.  Note if the runtime is not 64-bit it will only print an error.
SET CLASSPATH=..\libs\jars\*;XMLDBDriver.jar
IF NOT DEFINED JAVA_HOME FOR /F "delims=" %%O IN ('java XMLDBDriver --print-java-home') DO @SET JAVA_HOME=%%O

REM Check once again if we have found a JAVA_HOME.  If not it may be the case that we have no Java
REM or it is not 64-bit.  In this case an error message should have already been printed.
IF DEFINED JAVA_HOME (

REM Update the PATH to be able to find the Java dlls
SET PATH=%JAVA_HOME%\bin;%JAVA_HOME%\bin\server

REM Run GCAM
gcam.exe -C configuration.xml

)

pause
