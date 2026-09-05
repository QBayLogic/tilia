#!/usr/bin/env python3
"""Regenerate src/Tilia/Fixity/Builtin.hs from a GHC installation.

Usage:

    ./generate-builtin-fixities.py --ghc /path/to/ghc-9.14.1/bin/ghc

`--ghc` defaults to whatever `ghc` is on PATH. Nix users can point it at a
store path directly; the compiler needs no project or package environment.
"""

from __future__ import annotations

import argparse
import re
import shutil
import subprocess
import sys
from pathlib import Path

# The boot packages we take fixities from.
PACKAGES = [
    "array",
    "base",
    "binary",
    "bytestring",
    "containers",
    "deepseq",
    "directory",
    "exceptions",
    "filepath",
    "ghc-bignum",
    "ghc-boot-th",
    "ghc-internal",
    "ghc-prim",
    "hpc",
    "mtl",
    "os-string",
    "parsec",
    "pretty",
    "process",
    "stm",
    "template-haskell",
    "text",
    "time",
    "transformers",
    "unix",
]

# How many modules one GHCi process handles before we start a fresh one.
# Every module drags its interface (and its dependencies') into the session
# and nothing is ever released, so an unbounded session grows without limit.
CHUNK = 40

SYMBOL = r"[!#$%&*+./<=>?@\\^|~:-]+"
# The trailing hashes are MagicHash names such as unpackCString#: they read
# as identifiers, not operators, however many operator characters they end
# in.
IDENT = r"[A-Za-z_][A-Za-z0-9_']*#*"

# A qualifier has to be matched explicitly rather than stripped after the
# fact, because "." is itself an operator character: "Data.Function.." is
# the operator "." and not some operator named "..".
QUALIFIER = re.compile(r"[A-Z][A-Za-z0-9_']*\.")
NAME = rf"(?:[A-Z][A-Za-z0-9_']*\.)*({SYMBOL}|{IDENT})"

# Reserved syntax that GHC will nonetheless answer `:info` for, at
# precedences outside 0-9 that no source file can use.
RESERVED = {"->", "=>", "::", "=", "|", "\\", "<-", "..", "@", "~"}

OUTPUT = Path("src/Tilia/Fixity/Builtin.hs")

def run_ghci(ghc: Path, script: str) -> tuple[str, str]:
    """Feed a script to GHCi and return what it wrote to stdout and stderr."""
    ghci = ghc.with_name(ghc.name.replace("ghc", "ghci", 1))
    argv = [
        str(ghci),
        "-v0",
        "-ignore-dot-ghci",
        # Without this GHCi keeps Prelude in scope on top of whatever module
        # we asked for, which makes every operator Prelude also exports
        # ambiguous and answers for the wrong module.
        "-XNoImplicitPrelude",
        # `:info` parses the names it is given, and the boot packages export
        # plenty that only parse with these on.
        "-XMagicHash",
        "-XUnboxedTuples",
        "-XUnboxedSums",
    ]
    for package in PACKAGES:
        argv += ["-package", package]
    proc = subprocess.run(argv, input=script, capture_output=True, text=True)
    return proc.stdout, proc.stderr

# GHCi does not print its prompt when stdin is a pipe, so we emit our own
# markers by shelling out. That only stays in step with GHCi's own output if
# the session's handles are line buffered, hence the preamble.
PREAMBLE = (
    ":module + System.IO\n"
    "System.IO.hSetBuffering System.IO.stdout System.IO.LineBuffering\n"
    "System.IO.hSetBuffering System.IO.stderr System.IO.LineBuffering\n"
)

def marker(i: int) -> str:
    """Mark both streams, so that errors can be blamed on a command."""
    return f':!echo "@@ {i}"\n:!echo "@@ {i}" >&2\n'

def split_blocks(out: str, count: int) -> list[list[str]]:
    """Split marked GHCi output into one block of lines per marker."""
    blocks: list[list[str]] = [[] for _ in range(count)]
    current: list[str] | None = None
    for line in out.splitlines():
        m = re.fullmatch(r"@@ (\d+)", line)
        if m:
            current = blocks[int(m.group(1))]
        elif current is not None:
            current.append(line)
    return blocks

def chunked(xs: list, n: int):
    for i in range(0, len(xs), n):
        yield xs[i : i + n]

def exposed_modules(ghc: Path) -> list[str]:
    ghc_pkg = ghc.with_name(ghc.name.replace("ghc", "ghc-pkg", 1))
    modules: set[str] = set()
    for package in PACKAGES:
        proc = subprocess.run(
            [str(ghc_pkg), "field", package, "exposed-modules"],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
        )
        if proc.returncode != 0:
            sys.exit(f"{ghc_pkg} does not know about the package {package}")
        field = proc.stdout.split(":", 1)[1]
        # A re-export reads "Visible.Name from pkg-1.0:Original.Name"; the
        # visible name is the one an import will mention, so the two tokens
        # naming where it came from are dropped.
        tokens = field.replace(",", " ").split()
        i = 0
        while i < len(tokens):
            if tokens[i] == "from":
                i += 2
                continue
            modules.add(tokens[i])
            i += 1
    return sorted(modules)

def unescape(shown: str) -> str:
    """Undo the quoting GHCi puts around the names `:complete` reports."""
    return re.sub(r"\\(.)", r"\1", shown[1:-1])

# `:module` on its own empties the context, so a module that fails to load
# cannot leave the previous one in scope and answer for it.
def scope(module: str) -> str:
    return f":module\n:module {module}\n"

def exported_names(ghc: Path, modules: list[str]) -> dict[str, list[str]]:
    """List what each module exports, by asking what its scope completes to.

    `:complete` reports every name in scope, spelled both plainly and
    qualified; only the plain spellings are of interest here.
    """
    found: dict[str, list[str]] = {}
    for chunk in chunked(modules, CHUNK):
        script = PREAMBLE
        for i, module in enumerate(chunk):
            script += scope(module) + marker(i) + ':complete repl 100000 ""\n'
        out, _ = run_ghci(ghc, script)
        blocks = split_blocks(out, len(chunk))
        for module, block in zip(chunk, blocks):
            names = set()
            for line in block:
                if not line.startswith('"'):
                    continue  # the "how many of how many" header
                name = unescape(line)
                if not QUALIFIER.match(name):
                    names.add(name)
            found[module] = sorted(names - RESERVED)
    return found

# An alphanumeric name comes back in the backticks it would be written in.
FIXITY = re.compile(rf"^infix([lr]?) (\d) `?{NAME}`?$")

# How many names one `:info` command asks about. GHC abandons the rest of
# the command at the first name it dislikes, so a batch that complained
# about anything is halved and asked again rather than trusted.
BATCH = 32

# Names to ask about on top of what a module's scope completes to. GHCi's
# completion never offers ":", since a line starting with a colon is one of
# its own commands. The list constructor is wired into the compiler and so
# answers from any scope at all, which makes asking every module useless: it
# belongs where a source file will look for it, under the Prelude that every
# module imports whether it says so or not.
EXTRA = {"Prelude": [":"]}

def read_fixities(
    ghc: Path, exports: dict[str, list[str]]
) -> dict[str, dict[str, tuple[str, int]]]:
    """Collect the fixity declarations GHC reports for each module's scope."""
    table: dict[str, dict[str, tuple[str, int]]] = {m: {} for m in exports}
    for chunk in chunked(sorted(exports), CHUNK):
        # (module, names) pairs still to ask about, shrinking as they fail.
        pending = []
        for module in chunk:
            pending += [(module, b) for b in chunked(exports[module], BATCH)]
            if module in EXTRA:
                pending.append((module, EXTRA[module]))
        known = {m: set(exports[m]) | set(EXTRA.get(m, [])) for m in chunk}
        while pending:
            script = PREAMBLE
            for i, (module, batch) in enumerate(pending):
                query = " ".join(
                    name if re.fullmatch(IDENT, name) else f"({name})"
                    for name in batch
                )
                # The marker goes first so that a module which will not load
                # is blamed on its own batch and not on the one before it.
                script += marker(i) + scope(module) + f":info {query}\n"
            out, err = run_ghci(ghc, script)
            blocks = split_blocks(out, len(pending))
            failed = split_blocks(err, len(pending))
            retry = []
            for (module, batch), block, complaint in zip(pending, blocks, failed):
                # Deprecated modules warn on their way into scope; only a
                # real error means the answer was cut short.
                if any("error:" in line for line in complaint):
                    if len(batch) == 1:
                        continue  # not something `:info` will answer for
                    half = len(batch) // 2
                    retry.append((module, batch[:half]))
                    retry.append((module, batch[half:]))
                    continue
                for line in block:
                    m = FIXITY.match(line)
                    if m and m.group(3) in known[module]:
                        table[module][m.group(3)] = (m.group(1), int(m.group(2)))
            pending = retry
    # An operator GHC said nothing about has no fixity declaration, so it
    # takes the default. Alphanumeric names are only worth listing when they
    # were given a fixity, so they do not get the same treatment.
    for module, names in exports.items():
        for name in names:
            if not re.fullmatch(IDENT, name):
                table[module].setdefault(name, ("l", 9))
    return table

ASSOC = {"l": "LeftAssoc", "r": "RightAssoc", "": "NoAssoc"}

def escape(op: str) -> str:
    """Spell an operator as a Haskell string literal."""
    return op.replace("\\", "\\\\").replace('"', '\\"')

def render(table: dict[str, dict[str, tuple[str, int]]], version: str) -> str:
    entries = []
    for module in sorted(table):
        ops = table[module]
        head = f'    {"[" if not entries else ","} entry "{module}"'
        if not ops:
            entries.append(head + " []")
            continue
        rendered = ", ".join(
            f'("{escape(op)}", {ASSOC[d]}, {p})'
            for op, (d, p) in sorted(ops.items())
        )
        entries.append(head + "\n        [" + rendered + "]")
    body = "\n".join(entries)
    return f'''{{-# LANGUAGE OverloadedStrings #-}}

-- | Fixities of the operators that ship with the compiler.
--
-- Generated by @generate-builtin-fixities.py@ in the root of the
-- repository, from GHC {version}. Run that script again to update this table.
module Tilia.Fixity.Builtin
  ( builtinFixities,
  )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Tilia.Fixity

-- | Every module the boot packages expose, with the operators it exports.
builtinFixities :: Map Text (Map OpName Fixity)
builtinFixities =
  Map.fromList
{body}
    ]
  where
    entry name ops =
      (name, Map.fromList [(OpName o, Fixity d p) | (o, d, p) <- ops])
'''

def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--ghc",
        default=shutil.which("ghc"),
        help="the ghc to interrogate (default: the one on PATH)",
    )
    parser.add_argument(
        "--output",
        default=None,
        help=f"where to write the module (default: {OUTPUT})",
    )
    args = parser.parse_args()
    if not args.ghc:
        sys.exit("no ghc on PATH; pass --ghc")
    ghc = Path(args.ghc).resolve()
    output = Path(args.output) if args.output else Path(__file__).parent / OUTPUT

    version = subprocess.run(
        [str(ghc), "--numeric-version"], stdout=subprocess.PIPE, text=True
    ).stdout.strip()
    print(f"GHC {version}", file=sys.stderr)

    modules = exposed_modules(ghc)
    print(f"{len(modules)} exposed modules", file=sys.stderr)

    exports = exported_names(ghc, modules)
    print(
        f"{sum(len(v) for v in exports.values())} exported names",
        file=sys.stderr,
    )

    table = read_fixities(ghc, exports)
    print(f"{sum(len(v) for v in table.values())} fixities", file=sys.stderr)

    output.write_text(render(table, version))
    print(f"wrote {output}", file=sys.stderr)

if __name__ == "__main__":
    main()
