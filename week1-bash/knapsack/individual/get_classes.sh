#!/bin/bash
perl -0ne 'while (/class\s+(\S+).*?\{.*}/sg) {print "$1\n";}'
