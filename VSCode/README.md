# VSCode

## 拡張機能のインポート・エクスポート

### Linux

```bash
# エクスポート
code --list-extensions > extensions-list.txt

# インポート
cat extensions-list.txt | xargs -n 1 code --install-extension
```
