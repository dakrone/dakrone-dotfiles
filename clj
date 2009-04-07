#!/bin/bash 

CLOJURE_DIR=/home/hinmanm/src/clojure
CLOJURE_JAR=$CLOJURE_DIR/clojure.jar
CLOJURE_CONTRIB_JAR=/home/hinmanm/src/clojure-contrib/clojure-contrib.jar

# Add any additional jars for the classpath
ADDL_JARS=.:$CLOJURE_CONTRIB_JAR:$CLOJURE_DIR/estraier.jar

if [ -z "$1" ]; then 
      echo "java -cp $ADDL_JARS:$CLOJURE_DIR/jline-0.9.94.jar:$CLOJURE_JAR  jline.ConsoleRunner clojure.lang.Repl"
      java -cp $ADDL_JARS:$CLOJURE_DIR/jline-0.9.94.jar:$CLOJURE_JAR  jline.ConsoleRunner clojure.lang.Repl
else
      echo "java -cp $ADDL_JARS:$CLOJURE_JAR clojure.lang.Script $1"
      java -cp $ADDL_JARS:$CLOJURE_JAR clojure.lang.Script $1
fi

