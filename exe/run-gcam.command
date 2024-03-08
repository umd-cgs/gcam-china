#!/bin/sh

# Change to the appropriate working directory
DIR=`echo "'$0'" | xargs dirname`
DIR2=`echo "'$DIR'"`

eval cd $DIR2

# We need to find where the Java development kit is installed.
# This could be the Apple supplied version which was provided up
# to 1.6 however was dropped subsequently and instead users may
# have an Oracle provided JDK.  The each take slightly different
# approaches to where libraries live and how to reference them so
# we will have to try to detect the apporpriate location.

JAVA_HOME=$(/usr/libexec/java_home)
if [ -z "$JAVA_HOME" ]
then
    >&2 echo "ERROR: Could not find Java install location."
    exit 1
elif [ ${JAVA_HOME#*1.6} != $JAVA_HOME ]
then
    # The Apple supplied 1.6
    >&2 echo "ERROR: GCAM now requires Java 1.7+"
    exit 1
elif [[ ${JAVA_HOME#*jdk-9} != $JAVA_HOME || ${JAVA_HOME#*jdk-10} != $JAVA_HOME || ${JAVA_HOME#*jdk-11} != $JAVA_HOME ]]
then
    # The Oracle supplied Java 9/10 is slightly different
    LIB_PATH=${JAVA_HOME}/lib/server
else
    # The Oracle supplied Java 7/8
    LIB_PATH=${JAVA_HOME}/jre/lib/server
fi

# Create a symlink as well so that we can satisfy @rpath searches
if [ ! -h ../libs/java/lib ]
then
    ln -s ${LIB_PATH} ../libs/java/lib
fi

# We can now run GCAM
./Release/gcam -C configuration.xml
