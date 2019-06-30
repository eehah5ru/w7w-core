#!/bin/bash

if which md5 > /dev/null; then
    git diff | md5 | tr -d '\n'
else
    git diff | md5sum | tr -d '\n'
fi
