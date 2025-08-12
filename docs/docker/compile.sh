#!/bin/bash

set -e

cd /doc
pandoc -f markdown -t pdf -F mermaid-filter -o spec.pdf technical-specification.md
