#! /bin/bash 

if [[ "$#" -eq 0 ]]; then 
  echo 'error: requires an input file'
  exit 1
fi

echo 'Compiling plume file to arm assembly...'
if ! plume compile --arm "$1" > tmp.S;  then 
  rm tmp.S
  exit 1
fi

echo 'Assembling + linking arm assembly into executable'
arm-linux-gnueabihf-gcc -static -o a.out tmp.S
rm tmp.S

echo 'Done!'
