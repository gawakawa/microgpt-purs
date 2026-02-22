# CLAUDE.md

## Overview

Micro GPT implementation in PureScript (learning exercise based on Karpathy's microGPT).

- [Implementation](https://gist.github.com/karpathy/8627fe009c40f57531cb18360106ce95)
- [Explanation](https://karpathy.github.io/2026/02/12/microgpt/)

## Commands

- `nix fmt` - Format code (purs-tidy + nixfmt)
- `nix flake check` - Run checks (format, lint, tests via test-unit)
- `nix run` - Run the application
- `nix build -o output .#output` - Build compiled modules for LSP support (uses purs-nix, not spago)

## MCP

- **pursuit-mcp**: Search PureScript functions, types, and documentation on Pursuit
  1. Search by type signature first (e.g. `Array a -> Int`)
  2. Refine the type query to narrow down results
  3. If multiple similar functions are found, search for a promising one by name to get its details
  4. Only search by name when the type is completely unknown
