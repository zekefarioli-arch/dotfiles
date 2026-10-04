# Neovim (LazyVim)

[LazyVim](https://www.lazyvim.org/) with Catppuccin Mocha (pink accents) for
Java, Erlang, Elixir, TypeScript and front end, plus a git workflow.

## Requirements (Fedora)

```sh
sudo dnf install neovim ripgrep fd-find tree-sitter-cli gcc make unzip \
  java-25-openjdk-devel maven erlang erlang-rebar3 elixir nodejs npm
sudo dnf copr enable atim/lazygit && sudo dnf install lazygit
```

Plugins install on first start (pinned in `lazy-lock.json`); language servers
and formatters install through Mason (`:Mason`).

## Languages

| Language | Server | Extras |
|---|---|---|
| Java | jdtls | debugger (java-debug-adapter), tests (java-test) |
| Erlang | ELP (`lua/plugins/erlang.lua`; erlang_ls is archived) | |
| Elixir | ElixirLS | HEEx/EEx |
| TypeScript / JavaScript | vtsls | ESLint, Prettier, js-debug-adapter |
| Front end | html, cssls, emmet, tailwindcss, Vue, Svelte | color previews (mini-hipatterns) |
| JSON / YAML / TOML / Markdown / Docker | json, yaml, taplo, marksman, docker | |

## Everyday keys (`<leader>` = Space)

| Keys | Action |
|---|---|
| `<leader><space>` / `<leader>/` | find file / grep in project |
| `<leader>e` | file explorer |
| `gd` / `gr` / `K` | go to definition / references / hover docs |
| `<leader>ca` / `<leader>cr` | code action / rename |
| `<leader>cf` | format |
| `<leader>cs` | symbols outline |
| `<leader>xx` | diagnostics (Trouble) |
| `<leader>tt` / `<leader>tr` | run tests in file / nearest test |
| `<leader>db` / `<leader>dc` | breakpoint / start-continue debugger |

## Git workflow

| Keys | Action |
|---|---|
| `<leader>gg` | lazygit (stage, commit, push, rebase, branches) |
| `]h` / `[h` | next / previous changed hunk |
| `<leader>ghs` / `<leader>ghr` | stage / reset hunk |
| `<leader>ghp` | preview hunk |
| `<leader>gb` | blame line (inline blame is always on) |
| `<leader>gvo` / `<leader>gvc` | Diffview: open working tree diff / close |
| `<leader>gvf` / `<leader>gvr` | Diffview: file history / repo history |
| `<leader>gvm` | Diffview: current branch vs default branch |
| `<leader>gs` / `<leader>gl` | git status / log pickers |
| `:Octo pr list` / `:Octo issue list` | GitHub PRs and issues (uses `gh` auth) |

Press `<leader>` and wait to see every group in which-key.
