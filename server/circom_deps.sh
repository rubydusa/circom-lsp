#!/bin/bash

commit="ce903c6"

rm -rf circom
rm -rf parser
rm -rf type_analysis
rm -rf program_structure
git clone "https://github.com/iden3/circom"
cd circom
git checkout ${commit}
cd ..

mv circom/parser parser
mv circom/type_analysis type_analysis
mv circom/program_structure program_structure

rm -rf circom
