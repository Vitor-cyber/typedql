"""Testa que os arquivos em negativos/ NAO compilam, e pelo motivo certo.

Uso: python scripts/testar_negativos.py
"""

import pathlib
import shutil
import subprocess
import sys

RAIZ = pathlib.Path(__file__).resolve().parent.parent
STACK = shutil.which("stack") or "C:/Users/marvitox/haskell/bin/stack.exe"

# Nao basta o arquivo ser rejeitado: ele tem que ser rejeitado pelo motivo certo.
# Um import errado tambem faria o GHC falhar, e o teste passaria por acidente.
ESPERADO = {
    "ColunaInexistente.hs": "nao existe neste esquema",
    "AcessoAColunaInexistente.hs": "KnownIndex",
    "JuncaoAmbigua.hs": "juncao ambigua",
    "TipoErrado.hs": "TDouble",
}


def compilar(arquivo: pathlib.Path) -> subprocess.CompletedProcess:
    return subprocess.run(
        [
            STACK,
            "--stack-yaml",
            str(RAIZ / "stack.yaml"),
            "exec",
            "--",
            "ghc",
            "-fno-code",
            # usa a biblioteca ja compilada, para que o erro venha do arquivo
            # negativo e nao de recompilar src/ sem as extensoes do package.yaml
            "-package",
            "typedql",
            "-hide-all-packages",
            "-package",
            "base",
            "-package",
            "text",
            "-outputdir",
            str(RAIZ / ".stack-work" / "negativos"),
            str(arquivo),
        ],
        capture_output=True,
        text=True,
        cwd=str(RAIZ),
    )


def main() -> int:
    arquivos = sorted((RAIZ / "negativos").glob("*.hs"))
    if not arquivos:
        print("nenhum arquivo em negativos/")
        return 1

    falhas = []
    for arquivo in arquivos:
        res = compilar(arquivo)
        saida = res.stdout + res.stderr
        esperado = ESPERADO.get(arquivo.name)

        if res.returncode == 0:
            falhas.append(arquivo.name)
            print(f"[RUIM]  {arquivo.name}: compilou, mas deveria falhar")
            continue
        if esperado is None:
            falhas.append(arquivo.name)
            print(f"[RUIM]  {arquivo.name}: sem mensagem esperada em ESPERADO")
        elif esperado not in saida:
            falhas.append(arquivo.name)
            print(f"[RUIM]  {arquivo.name}: falhou, mas nao por {esperado!r}")
        else:
            print(f"[OK]    {arquivo.name}: rejeitado por {esperado!r}")

        for linha in saida.splitlines():
            if linha.strip():
                print("        " + linha)
        print()

    total = len(arquivos)
    if falhas:
        print(f"{len(falhas)} de {total} nao passaram: {falhas}")
        return 1
    print(f"{total} de {total} rejeitados corretamente.")
    return 0


if __name__ == "__main__":
    sys.exit(main())