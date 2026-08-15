"""Testa que os arquivos em negativos/ NAO compilam, e mostra a mensagem do GHC.

Uso: python scripts/testar_negativos.py
"""

import pathlib
import shutil
import subprocess
import sys

RAIZ = pathlib.Path(__file__).resolve().parent.parent
STACK = shutil.which("stack") or "C:/Users/marvitox/haskell/bin/stack.exe"


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
            "-XGHC2021",
            # usa a biblioteca ja compilada, para que o erro venha do arquivo
            # negativo e nao de recompilar src/ sem as extensoes do package.yaml
            "-package",
            "typedql",
            "-hide-all-packages",
            "-package",
            "base",
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
        if res.returncode == 0:
            falhas.append(arquivo.name)
            print(f"[RUIM]  {arquivo.name}: compilou, mas deveria falhar")
            continue
        print(f"[OK]    {arquivo.name}: rejeitado pelo GHC")
        for linha in (res.stdout + res.stderr).splitlines():
            if linha.strip():
                print("        " + linha)
        print()

    total = len(arquivos)
    if falhas:
        print(f"{len(falhas)} de {total} deveriam falhar e nao falharam: {falhas}")
        return 1
    print(f"{total} de {total} rejeitados corretamente.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
