#!/bin/bash

# matches:   // <comment> <end-of-string>
perl -pe 's/\/\/(.*?)$//'
