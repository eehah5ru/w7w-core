#!/bin/bash

if which md5; then
    git diff | md5
else
    git diff | md5sum
fi
