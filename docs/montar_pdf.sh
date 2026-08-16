#!/bin/sh
# Monta docs/relatorio.pdf: relatorio.md -> pandoc -> HTML+CSS -> Edge headless -> PDF
set -e
cd "$(dirname "$0")"

# Nenhum caminho depende de onde o repositorio foi clonado nem de qual usuario
# esta rodando: pandoc e o Edge sao procurados no PATH e nos locais padrao.
achar() {
  for c in "$@"; do
    if [ -x "$c" ] || command -v "$c" >/dev/null 2>&1; then echo "$c"; return 0; fi
  done
  return 1
}

PANDOC=$(achar pandoc "$HOME/.aki/bin/pandoc.exe" "/c/Program Files/Pandoc/pandoc.exe") \
  || { echo "pandoc nao encontrado. Instale: https://pandoc.org/installing.html" >&2; exit 1; }

EDGE=$(achar \
  "/c/Program Files (x86)/Microsoft/Edge/Application/msedge.exe" \
  "/c/Program Files/Microsoft/Edge/Application/msedge.exe" \
  "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe" \
  "C:/Program Files/Microsoft/Edge/Application/msedge.exe" \
  msedge chromium google-chrome) \
  || { echo "Edge ou Chrome nao encontrado (preciso de um navegador headless para gerar o PDF)" >&2; exit 1; }

"$PANDOC" relatorio.md -t html5 -o corpo.frag
python envolver.py
rm -f corpo.frag relatorio.pdf
"$EDGE" --headless --disable-gpu \
  --no-pdf-header-footer --print-to-pdf="$(pwd)/relatorio.pdf" \
  "file:///$(pwd)/relatorio.html" >/dev/null 2>&1
python contar_paginas.py
