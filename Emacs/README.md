# Emacs

## Requirements

- Emacs (31.1+)
- git
- Node.js
- npm
- gcc
- bash
- diff
- pip

## Setup (Common)

``` bash
# HTML / CSS / JSON
npm i -g vscode-langservers-extracted

# TypeScript / JavaScript
npm i -g typescript-language-server
# Run this in each project
npm i -D typescript@6 prettier eslint

# Vue.js
npm i -g @vue/language-server@2.2.0
# Run this in each project
npm i -D eslint-plugin-vue @vue/eslint-config-typescript

# SQL
pip install sqlparse --break-system-packages
```

## Setup (Windows)

``` bash
# MSYS2 UCRT64
pacman -Syu
pacman -S --needed base-devel mingw-w64-ucrt-x86_64-toolchain
```

下記へのパスを通す
- gcc (MSYS2 UCRT64, C:\msys64\ucrt64\bin)
- bash (Git Bash, C:\Program Files\Git\bin)
- diff (Git Bash, C:\Program Files\Git\usr\bin)
