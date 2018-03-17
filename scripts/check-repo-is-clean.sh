#!/bin/bash

if [[ -n $(git status --porcelain) ]]; then echo "dirty"; else "clean"; fi
