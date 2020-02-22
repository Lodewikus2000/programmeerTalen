#!/bin/bash

# matches:   /* <comment> */
perl -0pe 's/\/\*.*?\*\//\n/sg'
