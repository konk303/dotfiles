## Description

このコマンドは以下の作業を自動で実行します：

1. `devcontainer exec --workspace-folder . bin/rubocop -a` でRuboCopフォーマットを実行
2. 変更内容を適度な粒度でコミット
3. GitHub PRを作成

## Implementation

RuboCopをかけたあと、適切な粒度でコミットし、デフォルトブランチへマージするPRを作って

**使用例:** `/create-pr` → 自動でPR作成完了
