#!/bin/bash
JARFILE="target/scala-2.11/sor1k_2.11-1.0.opt.jar"
rm -rf disassembled
mkdir disassembled
for i in $(jar -tf "${JARFILE}" | grep class | sed 's/.class//g'); do
    mkdir -p "./disassembled/`dirname "$i"`"
    javap -classpath "${JARFILE}" -c -private "$i" > "disassembled/$i.txt"
done
#javap -classpath jruler.jar -s