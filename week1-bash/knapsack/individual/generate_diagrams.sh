#!/bin/bash

./generate_diagram_classes.sh      > diagram_classes.dot
./generate_diagram_extends.sh      > diagram_extends.dot
./generate_diagram_import.sh       > diagram_import.dot
./generate_diagram_composition.sh  > diagram_composition.dot
./generate_diagram_package.sh      > diagram_package.dot
./generate_diagram_all.sh          > diagram_all.dot
