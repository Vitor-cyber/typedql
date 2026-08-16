# Conta paginas do PDF gerado; o relatorio deve caber em uma.
import io, re, sys
d = io.open('relatorio.pdf', 'rb').read()
n = len(re.findall(rb'/Type\s*/Page[^s]', d))
print('paginas:', n, '| bytes:', len(d))
sys.exit(0 if n == 1 else 1)
