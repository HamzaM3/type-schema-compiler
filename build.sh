#!/bin/sh

cd src && ghc ./Main && cd ..
rm src/*.o src/*.hi
mkdir bin 2> /dev/null
mv src/Main bin/type-schema-compiler