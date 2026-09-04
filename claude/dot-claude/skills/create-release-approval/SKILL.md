---
name: create-release-approval
description: PRからNotionのDB_backlogにリリース承認資料を作成するスキル。「リリース承認資料を作って」「PR#XXXのリリース承認資料」などのタスクで発動する。引数にPR番号を取る。
---

# リリース承認資料の作成

引数: PR番号（例: `/create-release-approval 1244`）

## 手順

### Step 1: 既存チェック
- Notion DB_backlog（data_source: `collection://210b1b61-182d-80ae-80b1-000bafd48c73`）をworkspace_searchで検索し、対象PRのリリース承認資料が既に存在しないか確認する
- 存在する場合はユーザーに報告し、更新するか新規作成するか確認する

### Step 2: PR情報の取得
- `gh pr view {PR番号} --repo creal/creal-manager --json title,body,files` でPR情報を取得
- `gh pr diff {PR番号} --repo creal/creal-manager` で実際のdiffを取得

### Step 3: PR descriptionとdiffの整合性チェック（重要）
- PR descriptionに記載されている変更内容が、実際のdiffと一致しているか検証する
- 不一致がある場合（diffにない変更が記載されている、またはdiffにある変更が未記載）はユーザーに報告し、PR description修正を先に行う
- **descriptionの修正とリリース承認資料の作成は分けて行う**

### Step 4: 影響範囲の特定
- 変更されたgem/ファイルに基づき、アプリ内で影響を受ける機能をgrep/探索で特定する
- 主な観点:
  - 認証（Google OAuth2, Firebase Auth）
  - ファイル操作（GCS / Active Storage: Document, MediaResource）
  - CSV入出力（BOM|UTF-8エンコーディング: csv_downloadable, csv_bulk_upsertable）
  - PDF生成（Ferrum: rooms_controller等）
  - バックグラウンドジョブ（Sidekiq: app/jobs/配下）
  - 外部API連携（google-cloud-storage, firebase-auth等）

### Step 5: Notionにテンプレートからページ作成
DB_backlogにNotionテンプレートを使ってページを作成する:
- parent: `data_source_id: 210b1b61-182d-80ae-80b1-000bafd48c73`
- template_id: `ef6ecf25-d265-4ee4-9bad-002fd0bd0abd`（リリース承認資料テンプレート）
- プロパティ:
  - 名前: `\[タスク\]{PRの概要}`
  - ステータス: `アサイン依頼`
  - タスク: `内部`
  - プロダクト: `CMG`
  - 担当者: `2979deea-560a-4fa8-920b-2267ce2c4cc3` （TanakaJunki / junki.tanaka@creal.jp。UUID文字列で指定）
  - 予定開始日: 当日の日付（プロパティ名は `date:予定開始日→予定終了日:start`、ISO 8601形式 `YYYY-MM-DD`。`date:予定開始日→予定終了日:is_datetime` は `0`）
  - 予定終了日: 予定開始日の1ヶ月後（プロパティ名は `date:予定開始日→予定終了日:end`、ISO 8601形式 `YYYY-MM-DD`）
  - ※ DB_backlog の日付プロパティは `コード承認日` / `予定開始日→予定終了日` / `実績開始日→実績終了日` / `リリース承認日` の4つ。`開始日→終了日` という名前のプロパティは存在しないため、指定するとNotion APIが `validation_error` を返す
- **contentは指定しない**（テンプレートが非同期で適用される）

### Step 6: テンプレート適用後にコンテンツを更新
- テンプレート適用は非同期のため、少し待ってからfetchしてテンプレートが反映されたか確認する
- 反映後、update_contentでテンプレートの各セクションをPR情報と影響範囲に基づいて埋める
- **更新前に内容をユーザーに提示してレビューを受けること**

### Step 7: PR descriptionの先頭にNotionリンクを追加
- `gh pr view {PR番号} --repo creal/creal-manager --json body` で現在のPR description本文を取得
- 先頭に作成したNotionページへのリンクを以下の形式で追加する:
  ```
  リリース承認資料: <Notion URL>

  ---

  <既存の本文>
  ```
- 既にNotionリンクが先頭にある場合（再実行時など）は重複追加せず、URLを差し替える
- `gh pr edit {PR番号} --repo creal/creal-manager --body "..."` で更新

### Step 8: 完了報告
作成したページのURLとPR descriptionを更新した旨をユーザーに報告する。
