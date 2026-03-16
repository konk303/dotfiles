---
description: コードレビュー・RuboCopフォーマット後にコミットしてGitHub PRを作成する
---

このコマンドは以下の作業を順番に自動で実行します：

1. `/simplify` を実行して、変更コードのレビューと修正を行う
2. `devcontainer exec --workspace-folder . bin/rubocop -a` でRuboCopフォーマットを実行
3. 変更内容を適度な粒度でコミット
4. デフォルトブランチへマージするGitHub PRを作成
