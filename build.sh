#!/bin/bash
pandoc --filter pandoc-include-code cheatsheet.md -o dist/cheatsheet.pdf
