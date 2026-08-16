# Envolve o fragmento HTML do pandoc com o CSS de uma pagina A4.
import io
frag = io.open('corpo.frag', encoding='utf-8').read()
css = io.open('estilo_relatorio.css', encoding='utf-8').read()
html = ('<!DOCTYPE html><html lang="pt-BR"><head><meta charset="utf-8">'
        '<title>TypedQL</title><style>' + css + '</style></head><body>'
        + frag + '</body></html>')
io.open('relatorio.html', 'w', encoding='utf-8').write(html)
print('html:', len(html), 'bytes')
