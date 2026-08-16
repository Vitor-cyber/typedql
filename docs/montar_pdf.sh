#!/bin/sh
# Monta docs/relatorio.pdf: relatorio.md -> pandoc -> HTML+CSS -> Edge headless -> PDF
set -e
cd "$(dirname "$0")"
"C:/Users/marvitox/.aki/bin/pandoc.exe" relatorio.md -t html5 -o corpo.frag
python envolver.py
rm -f corpo.frag relatorio.pdf
"C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe" --headless --disable-gpu \
  --no-pdf-header-footer --print-to-pdf="$(pwd)/relatorio.pdf" \
  "file:///C:/Users/marvitox/Documents/typedql/docs/relatorio.html" >/dev/null 2>&1
python contar_paginas.py
