# Conta paginas do PDF gerado; o relatorio deve caber em uma.
import io, os, re, sys

# Aceita o caminho como argumento; por padrao usa o PDF ao lado deste script,
# para funcionar tanto da raiz do projeto quanto de dentro de docs/.
padrao = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'relatorio.pdf')
caminho = sys.argv[1] if len(sys.argv) > 1 else padrao
d = io.open(caminho, 'rb').read()
n = len(re.findall(rb'/Type\s*/Page[^s]', d))
print('paginas:', n, '| bytes:', len(d))
sys.exit(0 if n == 1 else 1)
