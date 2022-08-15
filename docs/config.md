---
layout: default
title: GNU Emacs configuration
nav_order: 1
---
# GNU Emacs configuration

## 1. はじめに
```note
* ここは [@minoruGH](https://twitter.com/minorugh)  の Emacs設定ファイルの一部を解説しているページです。
* <https://github.com/minorugh/emacs.d/> から
jekyll を使ってGithub pages にWebサイトを構築しています。
* 本ドキュメントは、[@takaxp](https://twitter.com/takaxp)さんが公開されている [takaxp.github.io/](https://takaxp.github.io/init.html) の記事を参考にした模倣版です。
* 執筆用途に特化してカスタマイズしていますので、コンセプトやキーバイドなどは極めて邪道思想になっています。
```
### 1.1 動作確認環境
以下の環境で使用しています。が、動作を保証するものではありません。

* Debian 11.4  86_64 GNU/Linux
* 自分でビルドした Emacs 27.2.50

### 1.2 デレクトリ構成
設定ファイルの構成は、下記のとおりです。

```
~/.emacs.d
│
├── el-get/
├── elpa/
├── inits/
│   ├── 00_base.el
│   ├── 01_dashboard.el
│   ├── ...
│   ├── 90_eshell.el
│   └── 99_chromium.el
├── snippets/
├── tmp/
├── early-init.el
├── init.el
└── mini-init.el

```

## 2. 起動設定
```note
Emacs-27導入にあわせて `early-init.el` を設定しました。 ブートシーケンスは以下のとおり。

1. `early-init.el` の読み込み
2. `init.el` の読み込み
3. `inits/` のファイル群を読み込み （init-loader 使用）
```

### 2.1 early-init-el
[`early-init.el`](https://github.com/minorugh/dotfiles/blob/main/.emacs.d/early-init.el) は、Emacs27から導入された早期初期化ファイルです。 

このファイルはパッケージシステムとGUIの初期化前にロードされるので、フレームの外見やpackage-enable-at-startup、package-load-list、package-user-dirのようなパッケージ初期化プロセスに影響を与える変数をカスタマイズできます。

### 2.2 [eary-init] 起動時間の短縮を図る
いままでinit.elに記述していたこれらの設定は、eary-init.elへ移したほうが起動時間を短縮できます。

```elisp
(push '(fullscreen . maximized) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
```

### 2.3 [eary-init] チラツキを抑える
設定ファイルの読み込み段階で画面がチラチラ変化するのを抑制しています。

```elisp
;; Suppress flashing at startup
(setq inhibit-redisplay t)
(setq inhibit-message t)
(add-hook 'window-setup-hook
		  (lambda ()
			(setq inhibit-redisplay nil)
			(setq inhibit-message nil)
			(redisplay)))

;; Startup setting
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq byte-compile-warnings '(cl-functions))
(custom-set-faces '(default ((t (:background "#282a36")))))
```

### 2.4 init.el
[`init.el`](https://github.com/minorugh/dotfiles/blob/main/.emacs.d/init.el) には、Packageの初期化設定とinit-lorderの設定を書いています。 

また、Magic File Name を一時的に無効にすることで、起動時間の短縮を図る設定をしています。

GC設定とともに設定ファイル読み込み後に正常値に戻します。

```emacs-lisp
(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000)))
```

### 2.5 [init-loader] 初期設定ファイルを読み込む
[`init-loader`](https://github.com/emacs-jp/init-loader/) は、設定ファイル群のローダーです。 指定されたディレクトリから構成ファイルをロードします。これにより、構成を分類して複数のファイルに分けることができます。

init-loader を使うことの是非については諸説あるようですが、[`多くの恩恵`](http://emacs.rubikitch.com/init-loader/) は捨て難く私には必須ツールです。

```emacs-lisp
(leaf init-loader
  :ensure t
  :config
  (custom-set-variables
   '(init-loader-show-log-after-init 'error-only))
  (init-loader-load))
```

### 2.6 mini-init.el
[`mini-init.el`](https://github.com/minorugh/dotfiles/blob/main/.emacs.d/mini-init.el) は、最小限の emacs を起動させるための設定です。

新しいパッケージや設定をテストしたり、エラー等で Emacsが起動しない場合に使用します。
シェルから `eq` と入力することで起動することがでます。

以下を `.zshrc` または `.bashrc` に書き込みます。

```shell
alias eq = 'emacs -q -l ~/.emacs.d/mini-init.el'
```

ファイルの PATH は、ご自分の環境に応じて修正が必要です。

## 3. コア設定
```note
Emacs を操作して文書編集する上で必要な設定。
```
### 3.1 言語 / 文字コード
シンプルにこれだけです。

``` emacs-lisp
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
```

### 3.2 [emacs-mozc] 日本語入力
Debian11 にインストールした Emacs上で [`emacs-mozc`](https://wiki.debian.org/JapaneseEnvironment/Mozc) を使っています。

Emacsをソースからビルドするときに `--without-xim` しなかったので、インライン XIMでも日本語入力ができてしまいます。
特に使い分けする必要もなく紛らわしいので `.Xresources` で XIM無効化の設定をしました。

```bash
! ~/.Xresources
! Emacs XIMを無効化
Emacs*useXIM: false

```

句読点などを入力したとき、わざわざ mozcに変換してもらう必要はないので以下を設定しておくことでワンアクションスピーディーになります。
```emacs-lisp
(leaf mozc
  :ensure t
  :bind (("<hiragana-katakana>" . toggle-input-method)
		 (:mozc-mode-map
		  ("," . (lambda () (interactive) (mozc-insert-str "、")))
		  ("." . (lambda () (interactive) (mozc-insert-str "。")))
		  ("?" . (lambda () (interactive) (mozc-insert-str "？")))
		  ("!" . (lambda () (interactive) (mozc-insert-str "！")))))
  :custom `((default-input-method . "japanese-mozc")
			(mozc-helper-program-name . "mozc_emacs_helper")
			(mozc-leim-title . "かな"))
  :config
  (defun mozc-insert-str (str)
	(mozc-handle-event 'enter)
	(insert str))
  (defadvice toggle-input-method (around toggle-input-method-around activate)
	"Input method function in key-chord.el not to be nil."
	(let ((input-method-function-save input-method-function))
	  ad-do-it
	  (setq input-method-function input-method-function-save))))
```

文章編集中にShellコマンドの [`mozc-tool`](https://www.mk-mode.com/blog/2017/06/27/linux-mozc-tool-command/) を起動して即、単語登録できるようにしておくと機能的です。

```elisp
(leaf *cus-mozc-tool
  :bind (("s-t" . my:mozc-dictionary-tool)
		 ("s-d" . my:mozc-word-regist))
  :init
  (defun my:mozc-dictionary-tool ()
	"Open `mozc-dictipnary-tool'."
	(interactive)
	(compile "/usr/lib/mozc/mozc_tool --mode=dictionary_tool")
	(delete-other-windows))

  (defun my:mozc-word-regist ()
	"Open `mozc-word-regist'."
	(interactive)
	(compile "/usr/lib/mozc/mozc_tool --mode=word_register_dialog")
	(delete-other-windows)))
```
### 3.3 フォント設定
メイン機（Thinkpad E590）とサブ機（Thinkpad X250）とで解像度が違うので設定を変えます。

```emacs-lisp
(add-to-list 'default-frame-alist '(font . "Cica-18"))
;; for sub-machine
(when (string-match "x250" (shell-command-to-string "uname -n"))
  (add-to-list 'default-frame-alist '(font . "Cica-15")))
```

### 3.4 基本キーバインド
Mac時代に慣れ親しんだ関係もあり、標準キーバインドの他に下記を追加しています。 

* `s-c` でコピー   (MacのCmd-c)
* `s-v` でペースト (Macの Cmd-v)

kill-bufferは、いちいち確認されるのが煩わしいので、kill-this-bufferを愛用しています。
```emacs-lisp
(global-set-key (kbd "M-/") 'my:kill-region')
```

`C-w` は、regionを選択していないときはカーソル行全体をkill-ringするようにしました。
```emacs-lisp
(defun my:kill-region ()
  "If the region is active, to kill region.
If the region is inactive, to kill whole line."
  (interactive)
  (if (use-region-p)
	  (clipboard-kill-region (region-beginning) (region-end))
    (kill-whole-line)))
(global-set-key (kbd "C-w") 'my:kill-region')
```

### 3.5 マウスで選択した領域を自動コピー
マウスで選択すると，勝手にペーストボードにデータが流れます．

```emacs-lisp
(setq mouse-drag-copy-region t)
```
### 3.6 C-x C-c でEmacsを終了させないようにする
Emacsを終了させることはまずないので、再起動コマンドに変更しています。
[`restart-emacs`](https://github.com/iqbalansari/restart-emacs) はMELPAからインストールできます。
```emacs-lisp
(leaf restart-emacs
  :ensure t
  :bind ("C-x C-c" . restart-emacs))
```

### 3.7 パッケージ管理
MELPAをメインに管理しています。MELPAにないものはel-getでGitHubやEmacsWikiからインストールします。

個人用に開発したものは、自分のGitHubリポジトリで管理しel-getで読み込んでいます。

## 4. カーソル移動
```note
ブラインドタッチは使わない（使えない）ので、文字移動、行移動、スクロールは、素直に上下左右の矢印キーと`PgUp` `PgDn` を使っています。
```

### 4.1 [sequential-command.el] バッファー内のカーソル移動
[`sequential-command`](https://github.com/HKey/sequential-command) は、バッファーの先頭と最終行への移動を簡単にしてくれます。

* `C-a` を連続で打つことで行頭→ファイルの先頭→元の位置とカーソルが移動
* `C-e` を連続で打つことで行末→ファイルの最終行→元の位置とカーソルが移動

地味なながら一度使うと便利すぎて止められません。

MELPAから Installできますが、私は HKey氏の改良版を el-getで使っています。
```emacs-lisp
(leaf sequential-command
  :doc "https://bre.is/6Xu4fQs6"
  :el-get HKey/sequential-command
  :config
  (leaf sequential-command-config
	:hook (emacs-startup-hook . sequential-command-setup-keys)))
```

### 4.2 ウインドウ間のカーソル移動
`C-c o` でもいいですが，ワンアクションで移動できるほうが楽なので、次のように双方向で使えるように設定しています．

画面分割されていないときは、左右分割して新しいウインドウに移動し、以後は双方向に移動します。

```emacs-lisp
(defun other-window-or-split ()
 "With turn on dimmer."
 (interactive)
 (when (one-window-p)
	 (split-window-horizontally)
	 (follow-mode 1)
	 (dimmer-mode 1))
   (other-window 1))
(global-set-key (kbd "C-q") 'other-window-or-split)
```

### 4.3 対応する括弧を選択
* `C-M-SPC` (mark-sexp) は，カーソル位置から順方向に選択．
* `C-M-U` (backward-up-list) は，一つ外のカッコの先頭にポイントを移す．

上記標準機能は使いにくいので、Vimの `%` の機能を実現させるために `my:jump-brace` を定義しました。
Toggleで括弧の先頭と最後にポイント移動します。
```emacs-lisp
(defun my:jump-brace ()
 "Jump to the corresponding parenthesis."
 (interactive)
 (let ((c (following-char))
	 (p (preceding-char)))
   (if (eq (char-syntax c) 40) (forward-list)
	 (if (eq (char-syntax p) 41) (backward-list)
       (backward-up-list)))))
(global-set-key (kbd "C-M-9") 'my:jump-brace)
(define-key view-mode-map (kbd "%") 'my:jump-brace)
```

### 4.4 [expand-region] カーソル位置を起点に選択範囲を賢く広げる
[`expand-region.el`](https://github.com/magnars/expand-region.el) は、カーソル位置を起点として前後に選択範囲を広げてくれます。

2回以上呼ぶとその回数だけ賢く選択範囲が広がりますが、2回目以降は設定したキーバインドの最後の一文字を連打すれば OKです。その場合、選択範囲を狭める時は - を押し， 0 を押せばリセットされます。

```emacs-lisp
(leaf expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))
```

## 5. 編集サポート
```note
ここにはファイル編集や入力補助の設定をまとめている。
```
### 5.1 矩形編集/連番入力
24.4 からは， rectangle-mark-mode が使えるようになり， C-x SPC を押下すると矩形モードに入り直感的に矩形選択ができる。

標準の rect.el に以下の機能が実装されている。

|矩形切り取り|	C-x r k |
|矩形削除	 |  C-x r d |
|矩形貼り付け|	C-x r y |
|矩形先頭に文字を挿入|	C-x r t |
|矩形を空白に変換する|	C-x r c |

### 5.2 C-x C-x で直前の編集ポイントへ行き来
`C-u C-SPC` も使いますが、直前の編集ポイントと現在のポイントとを行き来出来る設定を重宝しています。
```emacs-lisp
(defun my:exchange-point-and-mark ()
  "No mark active `exchange-point-and-mark'."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark))		 
(global-set-key (kbd "C-x C-x" 'my:kill-region'))
```

### 5.3 markdownモード
[`markdown-mode.el`](https://github.com/jrblevin/markdown-mode) は、Markdown形式のテキストを編集するための主要なモードです。

昨今は、`org-mode` の方が人気があるようですが、私の場合は、[Howm](https://howm.osdn.jp/index-j.html) でメモを書き、 [Hugo](https://github.com/gohugoio/hugo)でブログを書くので物書き環境は`markdown-mode` をメインにしています。

```emacs-lisp
(leaf markdown-mode
  :ensure t
  :mode ("\\.md\\'")
  :custom
  `((markdown-italic-underscore . t)
    (markdown-asymmetric-header . t)
	(markdown-fontify-code-blocks-natively . t))
```

markdownファイルのプレビューには、[`emacs-livedown`](https://github.com/shime/emacs-livedown) を使っています。
記事を書きながらライブでプレビュー出来るすぐれものです。

npmがインストールされたnodeが入っていことを確認してからlivedownをインストールします。
```shell
$ npm install -g livedown
```

次にEmacsの設定を書きます。
MELPAにはないのでel-getでインストールします。

```emacs-lisp
(leaf emacs-livedown
 :el-get shime/emacs-livedown
 :bind (("C-c C-c p" . livedown-preview)
        ("C-c C-c k" . livedown-kill)))
```

### 5.4 viewモード
特定の拡張子に対して常に view モードで開きたいときやgzされた elisp ソースを見るときに [view-mode](https://www.emacswiki.org/emacs/ViewMode) を使います。

下記の設定では、`my:auto-view-dirs` に追加したディレクトリのファイルを開くと `view-mode` が常に有効になります．

```elisp
(leaf view
  :hook
  (find-file-hook . my:auto-view)
  (server-visit-hook . my:unlock-view-mode)
  :bind
  (("S-<return>" . view-mode)
   (:view-mode-map
	("h" . backward-char)
	("l" . forward-char)
	("a" . beginning-of-buffer)
	("e" . end-of-buffer)
	("w" . forward-word)
	("b" . scroll-down)
	("c" . kill-ring-save)
	("r" . xref-find-references)
	("RET" . xref-find-definitions)
	("x" . my:view-del-char)
	("y" . my:view-yank)
	("d" . my:view-kill-region)
	("u" . my:view-undo)
	("m" . magit-status)
	("g" . my:google)
	("s" . swiper-region)
	("%" . my:jump-brace)
	("@" . counsel-mark-ring)
	("n" . my:org-view-next-heading)
	("p" . my:org-view-previous-heading)
	("o" . other-window-or-split)
	("G" . end-of-buffer)
	("0" . my:delete-window)
	("1" . my:delete-other-windows)
	("2" . my:split-window-below)
	("+" . text-scale-increase)
	("-" . text-scale-decrease)
	("." . (lambda ()(interactive)(text-scale-set 0)))
	("_" . kill-other-buffers)
	(":" . View-exit-and-edit)
	("i" . View-exit-and-edit)
	("]" . winner-undo)
	("[" . winner-redo)
	("," . hydra-view/body)))
  :init
  ;; Specific extension / directory
  (defvar my:auto-view-regexp "\\.php\\|\\.pl\\|\\.el.gz?\\|\\.tar.gz?\\'")

  ;; Specific directory
  (defvar my:auto-view-dirs nil)
  (add-to-list 'my:auto-view-dirs "~/src/")
  (add-to-list 'my:auto-view-dirs "~/Dropbox/GH/")
  (add-to-list 'my:auto-view-dirs "/scp:xsrv:/home/minorugh/")

  (defun my:auto-view ()
	"Open a file with view mode."
	(when (file-exists-p buffer-file-name)
	  (when (and my:auto-view-regexp
				 (string-match my:auto-view-regexp buffer-file-name))
		(view-mode 1))
	  (dolist (dir my:auto-view-dirs)
		(when (eq 0 (string-match (expand-file-name dir) buffer-file-name))
		  (view-mode 1)))))

  (defun my:unlock-view-mode ()
	"Unlock view mode with git commit."
	(when (string-match "COMMIT_EDITMSG" buffer-file-name)
	  (view-mode 0))))
```
`view-mode` のときにモードラインの色を変えるのは [`viewer.el`]() を使うと設定が簡単です。

```elisp
;; Change-modeline-color
(leaf viewer
  :ensure t
  :hook (view-mode-hook . viewer-change-modeline-color-setup)
  :custom `((viewer-modeline-color-view . "#852941")
	        (viewer-modeline-color-unwritable . "#2F6828")))
```

`view-mode` からでも簡単な編集ができるように `vim like` なコマンドをいくつか作りました。

```elisp
(with-eval-after-load 'view
  ;; save-buffer no message
  (defun my:save-buffer ()
	"With clear Wrote message."
	(interactive)
	(save-buffer)
	(message nil))

  ;; Like as 'x' of vim
  (defun my:view-del-char ()
	"Delete charactor in view mode."
	(interactive)
	(view-mode 0)
	(delete-char 1)
	(my:save-buffer)
	(view-mode 1))

  ;; Like as 'dd' of vim
  (defun my:view-kill-region ()
	"If the region is active, to kill region.
If the region is inactive, to kill whole line."
	(interactive)
	(view-mode 0)
	(if (use-region-p)
		(kill-region (region-beginning) (region-end))
	  (kill-whole-line))
	(my:save-buffer)
	(view-mode 1))

  ;; Like as 'u' of vim
  (defun my:view-undo ()
	"Undo in view mode."
	(interactive)
	(view-mode 0)
	(undo)
	(my:save-buffer)
	(view-mode 1))

  ;; Like as 'y' of vim
  (defun my:view-yank ()
	"Yank in view mode."
	(interactive)
	(view-mode 0)
	(yank)
	(my:save-buffer)
	(view-mode 1))

  ;; Like as '%' of vim
  (defun my:jump-brace ()
	"Jump to the corresponding parenthesis."
	(interactive)
	(let ((c (following-char))
		  (p (preceding-char)))
	  (if (eq (char-syntax c) 40) (forward-list)
		(if (eq (char-syntax p) 41) (backward-list)
		  (backward-up-list)))))

  (defun my:org-view-next-heading ()
	"Org-view-next-heading."
	(interactive)
	(if (and (derived-mode-p 'org-mode)
			 (org-at-heading-p))
		(org-next-visible-heading 1)
	  (next-line)))

  (defun my:org-view-previous-heading ()
	"Org-view-previous-heading."
	(interactive)
	(if (and (derived-mode-p 'org-mode)
			 (org-at-heading-p))
		(org-previous-visible-heading 1)
	  (previous-line))))
```

### 5.5 web/htmlモード
HTML編集をするなら[web-mode](https://github.com/fxbois/web-mode) がお勧めなのですが、私の場合あまり使っていません。

textファイルからコンパイラを通してHTMLを生成したり、markdownで書いてHTMLに変換するというケースが多く、無地のファイルからHTMLタグを書き始めると言うとがないからです。

出来上がったHTMLを部分的に変更したり...という程度の使い方です。

```elisp
(leaf web-mode
  :ensure t
  :mode ("\\.js?\\'" "\\.html?\\'" "\\.php?\\'")
  :custom
  `((web-mode-markup-indent-offset . 2)
	(web-mode-css-indent-offset . 2)
	(web-mode-code-indent-offset . 2)))
```

### 5.6 [darkroom-mode] 執筆モード
[`darkroom.el`](https://github.com/joaotavora/darkroom)  は、画面の余計な項目を最小限にして、文章の執筆に集中できるようにするパッケージです。

タイトルバーやモードラインが一時的に削除されてフルスクリーンになり、テキストが拡大され、テキストがウィンドウの中央に配置されるように余白が調整されます。`view-mode, diff-hl-mode, display-line-numbers-mode` をOffにし、行間も少し大きくしてより読みやすくしています。

[F12] キーで IN/OUT をトグルしています。
darkroom-modeからでるときは、revert-buffer で再読込してもとに戻しますが yes/noを聞いてくるのが煩わしいのでno-confirm の関数を作りました。

```emacs-lisp
(leaf darkroom
  :ensure t
  :bind (("<f12>" . my:darkroom-in)
		 (:darkroom-mode-map
		  ("<f12>" . my:darkroom-out)))
  :config
  (defun my:darkroom-in ()
	"Enter to the `darkroom-mode'."
	(interactive)
	(view-mode 0)
	(diff-hl-mode 0)
	(display-line-numbers-mode 0)
	(darkroom-tentative-mode 1)
	(setq line-spacing 0.4))

  (defun my:darkroom-out ()
	"Returns from `darkroom-mode' to the previous state."
	(interactive)
	(setq line-spacing 0.1)
	(darkroom-tentative-mode 0)
	(display-line-numbers-mode 1)
	(revert-buffer-no-confirm))

  (defun revert-buffer-no-confirm ()
	"Revert buffer without confirmation."
	(interactive)
	(revert-buffer t t)))
```

### 5.7 [yatex] YaTexで LaTex編集
[`yatex.el`](https://github.com/emacsmirror/yatex) は、Emacsの上で動作する LaTeX の入力支援環境です。

ごく一般的な設定例ですが、参考になるとしたら [`yatexprc.el`](https://www.yatex.org/gitbucket/yuuji/yatex/blob/c45e2a0187b702c5e817bf3023816dde154f0de9/yatexprc.el) の `M-x YaTeX-lpr` を使って一気に PDF作成まで自動化している点でしょうか。

```emacs-lisp
(leaf yatex
  :ensure t
  :mode ("\\.tex\\'" "\\.sty\\'" "\\.cls\\'")
  :custom `((tex-command . "platex")
			(dviprint-command-format . "dvpd.sh %s")
			(YaTeX-kanji-code . nil)
			(YaTeX-latex-message-code . 'utf-8)
			(YaTeX-default-pop-window-height . 15))
  :config
  (leaf yatexprc
	:bind (("M-c" . YaTeX-typeset-buffer)
		   ("M-v" . YaTeX-lpr))))
```
`YaTeX-lpr` は、`dviprint-command-format` を呼び出すコマンドです。

dviファイルから dvipdfmx で PDF作成したあと、PDFビューアーを起動させて表示させるところまでをバッチファイルに書き、PATHの通ったところに置きます。私は、`/usr/loca/bin` に置きました。

```sh
#!/bin/bash
name=$1
dvipdfmx $1 && evince ${name%.*}.pdf
# Delete unnecessary files
rm *.au* *.dv* *.lo*
```
上記の例では、ビューアーに Linux の evince を設定していますが、mac でプレビューを使う場合は、下記のようになるかと思います。

```sh
dvipdfmx $1 && open -a Preview.app ${name%.*}.pdf
```

### 5.8 [yasunippet] Emacs用のテンプレートシステム
テンプレート挿入機能を提供してくれるやつです。
```elisp
(leaf yasnippet
  :ensure t
  :hook (after-init-hook . yas-global-mode)
  :config
  (leaf yasnippet-snippets :ensure t))
```

以下の設定を追加すると[`company-mode`](https://github.com/company-mode/company-mode) と連携してとても使いやすくなる。
```elisp
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	  backend
	(append (if (consp backend) backend (list backend))
    	    '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
(global-set-key (kbd "C-<tab>" 'company-yasunippets))
```

### 5.9 [iedit] 選択領域を別の文字列に置き換える
[`idet.el`](https://github.com/victorhge/iedit) は、複数箇所を同時に編集するツールです。

同じような機能のものは、複数あるようですが、わたしはこれを愛用しています。
* [`multi-cursors.el`](https://github.com/magnars/multiple-cursors.el) 
* [`replace-from-region.el`](https://www.emacswiki.org/emacs/replace-from-region.el) 
* [`anzu.el`](https://github.com/emacsorphanage/anzu) 

MELPAからpackage-installするだけで使えます。

対象範囲を選択して `C-;` を押すとiedit-modeとなり、選択したキーワードが全てハイライト表示され、モードラインに押すとIedit:とキーワードの出現した回数が表示され、ミニバッファにもメッセージが表示されます。

ここで、ハイライトされた部分を編集すると、他のハイライトも同時に編集されるようになります。編集後、もう一度 `C-;` を押すと確定されiedet-modeを抜けます。

私の場合かなりの頻度で使うので、Emacsでは使うことのない `<insert>` にキーバインドしています。

```elisp
(leaf iedit
  :ensure t
  :bind ("<insert>" . iedit-mode))
```

### 5.10 [selected] リージョン選択時のアクションを制御
[`selected.el`](https://github.com/Kungsgeten/selected.el) は、選択領域に対するスピードコマンドです。

Emacsバッファーで領域を選択した後、バインドしたワンキーを入力するとコマンドが実行されます。
コマンドの数が増えてきたら、ヘルプ代わりに使える [`counsel-selected`](https://github.com/takaxp/counsel-selected) も便利そうです。
```emacs-lisp
(leaf selected
  :ensure t
  :hook (after-init-hook . selected-global-mode)
  :bind (:selected-keymap
		 (";" . comment-dwim)
		 ("c" . clipboard-kill-ring-save)
		 ("s" . swiper-thing-at-point)
		 ("t" . google-translate-auto)
		 ("T" . chromium-translate)
		 ("W" . my:weblio)
		 ("k" . my:koujien)
		 ("e" . my:eijiro)
		 ("g" . my:google)))
```

### 5.11 [selected] browse-urlで検索サイトで開く
検索結果を browse-url で表示させるユーザーコマンドは、検索 urlのフォーマットとさえわかれば、パッケージツールに頼らずともお好みのマイコマンドを作成できます。

```emacs-lisp
(defun my:koujien (str)
  (interactive (list (my:get-region nil)))
  (browse-url (format "https://sakura-paris.org/dict/広辞苑/prefix/%s"
                      (upcase (url-hexify-string str)))))

(defun my:weblio (str)
  (interactive (list (my:get-region nil)))
  (browse-url (format "https://www.weblio.jp/content/%s"
	                  (upcase (url-hexify-string str)))))

(defun my:eijiro (str)
  (interactive (list (my:get-region nil)))
  (browse-url (format "https://eow.alc.co.jp/%s/UTF-8/"
                      (upcase (url-hexify-string str)))))

(defun my:google (str)
	(interactive (list (my:get-region nil)))
	(browse-url (format "https://www.google.com/search?hl=ja&q=%s"
						(upcase (url-hexify-string str)))))

(defun my:get-region (r)
	"Get search word from region."
	(buffer-substring-no-properties (region-beginning) (region-end)))
```

### 5.12 [selected] IME のオン・オフを自動制御する
selectedコマンドを選択するときは、IMEをOffにしないといけないのですがこれを自動でさせます。

領域を選択し始める時に IMEをオフにして、コマンド発行後に IMEを元に戻すという例が、
[`@takaxp`](https://qiita.com/takaxp) さんの [`Qiitaの記事`](https://qiita.com/takaxp/items/00245794d46c3a5fcaa8) にあったので、私の環境（emacs-mozc ）にあうように設定したら、すんなり動いてくれました。感謝！

```emacs-lisp
(leaf *cus-selected
  :hook ((activate-mark-hook . my:activate-selected)
		 (activate-mark-hook . (lambda () (setq my:ime-flag current-input-method) (my:ime-off)))
		 (deactivate-mark-hook . (lambda () (unless (null my:ime-flag) (my:ime-on)))))
  :init
  ;; Control mozc when seleceted
  (defun my:activate-selected ()
	(selected-global-mode 1)
	(selected--on)
	(remove-hook 'activate-mark-hook #'my:activate-selected))
  (add-hook 'activate-mark-hook #'my:activate-selected)
  (defun my:ime-on ()
	(interactive)
	(when (null current-input-method) (toggle-input-method)))
  (defun my:ime-off ()
	(interactive)
	(inactivate-input-method))

  (defvar my:ime-flag nil)
  (add-hook
   'activate-mark-hook
   #'(lambda ()
	   (setq my:ime-flag current-input-method) (my:ime-off)))
  (add-hook
   'deactivate-mark-hook
   #'(lambda ()
	   (unless (null my:ime-flag) (my:ime-on)))))
```

### 5.13 [swiper-migemo] swiperを migemo化してローマ字入力で日本語を検索
[`avy-migemo-e.g.swiper.el`](https://github.com/momomo5717/avy-migemo) を使って出来ていたのですが、２年ほど前から更新が止まってしまっていて動きません。

つい最近、avy-migemo を使わない [`swiper-migemo`](https://github.com/tam17aki/swiper-migemo)を GitHubで見つけたので試した処、機嫌よく動いてくれています。
MELPAにはアップされていないみたいなので el-get で取得しています。

```emacs-lisp
(leaf swiper-migemo
  :el-get tam17aki/swiper-migemo
  :global-minor-mode t)
```

### 5.14 [smartparent.el] 対応する括弧の挿入をアシスト


## 6. 表示サポート
```note
ここでは Emacs の UI を変更するようなものを載せている。
```

### 6.1 cleanup-for-spaces

`whitespace` の設定はシンプルに `show-trailing-whitespace` のみとし、不用意に入ってしまったスペースを削除するための関数を設定しました。

```emacs-lisp
(leaf whitespace
  :ensure t
  :bind ("C-c C-c" . my:cleanup-for-spaces)
  :hook (prog-mode-hook . my:enable-trailing-mode)
  :config
  (setq show-trailing-whitespace nil)
  :init
  (defun my:enable-trailing-mode ()
    "Show tail whitespace."
    (setq show-trailing-whitespace t))

  (defun my:cleanup-for-spaces ()
    "Remove contiguous line breaks at end of line + end of file."
    (interactive)
    (delete-trailing-whitespace)
    (save-excursion
      (save-restriction
		(widen)
		(goto-char (point-max))
		(delete-blank-lines)))))

```
### 6.2 [volatile-highlight]コピペした領域を強調
コピペした領域をフラッシングさせます。

```emacs-lisp
(leaf volatile-highlights :ensure t
  :config
  (volatile-highlights-mode)
  (with-no-warnings
    (when (fboundp 'pulse-momentary-highlight-region)
      (defun my-vhl-pulse (beg end &optional _buf face)
		"Pulse the changes."
		(pulse-momentary-highlight-region beg end face))
      (advice-add #'vhl/.make-hl :override #'my-vhl-pulse))))
```

### 6.3 rainbow-mode

rainbow-mode.el は red, greenなどの色名や #aabbcc といったカラーコードから実際の色を表示するマイナーモードです。
常時表示しているとうざいとケースのあるので、必要なときだけ使えるようにしています。

```emacs-lisp
(leaf rainbow-mode
  :ensure t
  :bind ("C-c r" . rainbow-mode))
```
### 6.4 custom-set-face
色設定が、あちこちに散らばっているとわかりにくので、まとめて設定するようにしています。

```emacs-lisp
(custom-set-faces
 '(lsp-face-highlight-read ((t (:background "gray21" :underline t))))
 '(lsp-face-highlight-write ((t (:background "gray21" :underline t))))
 '(markdown-code-face ((t (:inherit nil))))
 '(markdown-pre-face ((t (:inherit font-lock-constant-face))))
 '(markup-meta-face ((t (:stipple nil :foreground "gray30" :inverse-video nil :box nil
								  :strike-through nil :overline nil :underline nil :slant normal
								  :weight normal :height 135 :width normal :foundry "unknown" :family "Monospace"))))
 '(symbol-overlay-default-face ((t (:background "gray21" :underline t))))
 '(mozc-cand-posframe-normal-face ((t (:background "#282D43" :foreground "#C7C9D1"))))
 '(mozc-cand-posframe-focused-face ((t (:background "#393F60" :foreground "#C7C9D1"))))
 '(mozc-cand-posframe-footer-face ((t (:background "#282D43" :foreground "#454D73")))))
(put 'dired-find-alternate-file 'disabled nil)
```

## 7. Hydraで「鬼軍曹」から逃避
```note
[hydra.el](https://github.com/abo-abo/hydra) を使うとよく使う機能をまとめてシンプルなキーバインドを割り当てることができます。

日本では、[smartrep.el](http://sheephead.homelinux.org/2011/12/19/6930/) が有名だったようですが、hydra.elも同様の機能を提供します。
```
むかし、Emacsのキーバインドを強制する [鬼軍曹.el](https://github.com/k1LoW/emacs-drill-instructor) というパッケージがありました。
その後、
[`key-chord.el`]() や [`hydra.el`]() などが開発されてキーバインドの枯渇化が解消され、自由度が向上しました。


私は下記の `hydra` を設定しています。

* [hydra-work-menu](https://live.staticflickr.com/65535/50175364331_9fcf3c6c86_b.jpg) 
* [hydra-quick-menu](https://live.staticflickr.com/65535/50174826063_b4fa442b1e_b.jpg) 
* [hydra-make](https://github.com/minorugh/dotfiles/blob/31fbe8f956d453db9804e60f1a244919c6876689/.emacs.d/inits/20_hydra-make.el#L5) 
* [hydra-package](https://github.com/minorugh/dotfiles/blob/31fbe8f956d453db9804e60f1a244919c6876689/.emacs.d/inits/20_hydra-misc.el#L6) 
* [hydra-browse](https://github.com/minorugh/dotfiles/blob/31fbe8f956d453db9804e60f1a244919c6876689/.emacs.d/inits/20_hydra-misc.el#L33) 
* [hydra-markdown](https://github.com/minorugh/dotfiles/blob/31fbe8f956d453db9804e60f1a244919c6876689/.emacs.d/inits/40_markdown.el#L18) 
* [hydra-view-mode](https://github.com/minorugh/dotfiles/blob/88667e20b779d8dfd8d73895538299d6f7feaba8/.emacs.d/inits/40_view-mode.el#L83) 


また、それぞれ下記のような相関になっています。
```

┌──────────────────┐
│ hydra-work-menu  │ ワークテーブル分岐
└──────────────────┘
￬￪ 相互に行き来できる
┌──────────────────┐
│ hydra-quick-menu │ よく使うコマンド群
└──────────────────┘
   │
   ├── hydra-compile
   ├── hydra-markdown
   ├── hydra-package
   ├── hydra-magit    <<- Dired からも呼び出せる
   ├── hydra-browse   <<- Dashboard からも呼び出せる
   └── hydra-view-mode
```

### 7.1 [hydra-menu] 作業コマンドメニュー 
[`hydra-work-menu](https://github.com/minorugh/dotfiles/blob/31fbe8f956d453db9804e60f1a244919c6876689/.emacs.d/inits/20_hydra-menu.el#L57) には、
ブログ記事のほかWEB日記や俳句関係のシリーズ記事の追加、編集など、毎日頻繁に開くワークスペースへのショートカットを設定しています。

![hydra-work-menu](https://live.staticflickr.com/65535/50175364331_9fcf3c6c86_b.jpg) 

[hydra-quick-menu](https://github.com/minorugh/dotfiles/blob/31fbe8f956d453db9804e60f1a244919c6876689/.emacs.d/inits/20_hydra-menu.el#L5) の方には、
編集作業で頻繁に使うツール群のほか、my:dired でプロジェクトのディレクトリを一発で開くためのショートカットなどを設定しています。

![hydra-quick-menu](https://live.staticflickr.com/65535/50174826063_b4fa442b1e_b.jpg) 


この２つの hydra は、いわば私の秘書のような役割で、どちらからでも相互にトグルで呼び出せるようにしています。

### 7.2 その他の Hydra 設定。
hydra で工夫するといろんなコマンドのキーバインドを記憶する必要もなく GUI 感覚で操作できるので積極的に使っています。

Qitta に詳しい記事を書いています。

* [Hydraで Emacsのキーバインド問題を解消](https://qiita.com/minoruGH/items/3776090fba46b1f9c228)


## 8. 履歴 / ファイル管理

### 8.1 [auto-save-buffer-enhanced] ファイルの自動保存

[auto-save-buffer-enhanced.el](https://github.com/kentaro/auto-save-buffers-enhanced) は、Emacs に本当の自動保存機能を提供します。

Tramp-mode と併用すると emacs が固まってしまうことがあるようなので、 `auto-save-buffers-enhanced-exclude-regexps` を設定して trampでリモート編集時には auto-save-buffers を停止するようにしています。

また、このパッケージには、scratchバッファーの内容も保存してくれるので併せ設定している。

```emacs-lisp
(leaf auto-save-buffers-enhanced
  :ensure t
  :config
  (setq auto-save-buffers-enhanced-exclude-regexps '("^/ssh:" "^/scp:" "/sudo:"))
  (setq auto-save-buffers-enhanced-quiet-save-p t)
  (setq auto-save-buffers-enhanced-save-scratch-buffer-to-file-p t)
  (setq auto-save-buffers-enhanced-file-related-with-scratch-buffer "~/.emacs.d/tmp/scratch")
  (auto-save-buffers-enhanced t)
  (defun read-scratch-data ()
	(let ((file "~/.emacs.d/tmp/scratch"))
	  (when (file-exists-p file)
		(set-buffer (get-buffer "*scratch*"))
		(erase-buffer)
		(insert-file-contents file))))
  (read-scratch-data))
```

### 8.2 空になったファイルを自動的に削除

howm や org でメモをとるときに、ゴミファイルが残らないように時々メンテしています。ファイルを開いて中味を確認してから、一度閉じて dited で削除するというプロセスは手間がかかりすぎます。

下記の設定をしておくと、`C-x h` で全選択して delete したあと `kill-buffer` することで自動的にファイルが削除されるので便利です。

```emacs-lisp
(defun my:delete-file-if-no-contents ()
  "Automatic deletion for empty files (Valid in all modes)."
  (when (and (buffer-file-name (current-buffer))
			 (= (point-min) (point-max)))
    (delete-file
     (buffer-file-name (current-buffer)))))
(if (not (memq 'my:delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
		  (cons 'my:delete-file-if-no-contents after-save-hook)))

```

### 8.3 [undo-fu] シンプルな undo/redo を提供
[undo-fu](https://github.com/emacsmirror/undo-fu)  はシンプルな undo/redo 機能を提供してくれるやつです。

昔はもっと色々できる [undo-tree](https://github.com/apchamberlain/undo-tree.el)  を使っていたけどそっちにバグがあるっぽいので乗り換えました。

```emacs-lisp
(leaf undo-fu
  :ensure t
  :bind (("C-_" . undo-fu-only-undo)
		 ("C-/" . undo-fu-only-redo)))
```


## 9. 開発サポート

### 9.1 dotfiles
dotfilesとは、ホームディレクトリに置いてあるドット [.] から始まる設定ファイル（.bashrcとか）を管理しているリポジトリのことで、環境再構築に必要なシェルやエディタの設定からアプリケーションの設定まで幅広いものが置かれていて、当然ながら、[.emacs.d] の中身も含まれています。

<a href="https://minorugh.github.io/docs/dotfiles.html" class="btn">詳細はこちらにまとめました <i class="fa fa-arrow-circle-right"></i></a>


### 9.2 [magit]

magit status の画面は、デフォルトでは `other-window` に表示されますが、フルフレームで表示されるようにカスタマイズしています。

```emacs-lisp
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
```
### 9.3 gist-from-buffer
ターミナルからコマンドラインで使える `gist` を使って Emacsで開いているバッファーから `gist` できるように関数を作りました。
`gist -o` とすることで結果の URLをブラウザで開いてくれるので便利です。

```emacs-lisp
  (defun gist-from-buffer ()
	"Gist from current buffer, then open chromium."
	(interactive)
	(let ((file (buffer-file-name (current-buffer))))
	  (compile (concat "gist -o " file)))
	(delete-other-windows))
```


## 10. メモ環境

### 10.1 Howm Mode
私の場合、専門的なプログラミングもやらないし、余生の身なのでGTDとかも必要ありません。
Emacsはメモ書きに特化した使い方なので、`Howm` を使って markdown記法で書いています。

```emacs-lisp
(leaf howm
  :ensure t
  :hook (emacs-startup-hook . howm-mode)
  :chord ("@@" . howm-list-all)
  :init
  (setq howm-view-title-header "#")
  (defun my:howm-create-file ()
    "Make howm create file with 'org-capture'."
    (interactive)
    (format-time-string "~/Dropbox/howm/%Y/%m/%Y%m%d%H%M.md" (current-time)))
  :config
  (bind-key [backtab] 'howm-view-summary-previous-section howm-view-summary-mode-map)
  (setq howm-directory "~/Dropbox/howm")
  (setq howm-view-split-horizontally t)
  (setq howm-view-summary-persistent nil)
  (setq howm-normalizer 'howm-sort-items-by-reverse-date)
  (setq howm-user-font-lock-keywords
		'(("memo:" . (0 'gnus-group-mail-3))
		  ("note:" . (0 'epa-mark))
		  ("perl:" . (0 'diff-refine-added))
		  ("haiku:" . (0 'compilation-mode-line-exit))
		  ("emacs:" . (0 'compilation-info))
		  ("linux:" . (0 'compilation-error)))))
```

### 10.2 Org Mode
dashboard画面に簡単なタスクを表示させるために `org-agenda` を使っています。

ついでなので `org-capture` からHowmメモを発動できるようにTemplateを作りました。
ただ、captureからだと画面が半分になるのがいやなので、最大化で開くようにしています。

```elisp
(leaf org
  :hook (emacs-startup-hook . (lambda () (require 'org-protocol)))
  :chord (",," . org-capture)
  :bind (("C-c a" . org-agenda)
		 ("C-c c" . org-capture)
		 ("C-c k" . org-capture-kill)
		 ("C-c o" . org-open-at-point)
		 ("C-c i" . org-edit-src-exit)
		 (:org-mode-map
		  ("C-c i" . org-edit-special)))
  :custom `((org-log-done . 'org)
			(timep-use-speed-commands . t)
			(org-src-fontify-natively . t)
			(org-startup-indented . t)
			(org-hide-leading-stars . t)
			(org-startup-folded . 'content)
			(org-indent-mode-turns-on-hiding-stars . nil)
			(org-indent-indentation-per-level . 4)
			(org-startup-folded . 'content)
			(org-agenda-files . '("~/Dropbox/org/task.org"))
			(org-agenda-span . 30))
  :config
  (defun my:howm-create-file ()
    "Make howm create file with 'org-capture'."
    (interactive)
    (format-time-string "~/Dropbox/howm/%Y/%m/%Y%m%d%H%M.md" (current-time)))
  ;; Caputure Settings
  (setq org-capture-templates
		'(("m" " Memo with howm" plain (file my:howm-create-file)
		   "# memo: %?\n%U %i")
		  ("n" " Note with howm" plain (file my:howm-create-file)
		   "# note: %?\n%U %i")
		  ("t" " Task" entry (file+headline "~/Dropbox/org/task.org" "TASK")
		   "** TODO %?\n SCHEDULED: %^t \n" :empty-lines 1 :jump-to-captured 1)
		  ("e" " Experiment Perl" entry (file+headline "~/Dropbox/org/experiment.org" "Experiment")
		   "* %? %i\n#+BEGIN_SRC perl\n\n#+END_SRC\n\n%U")
		  ("p" " Code capture" entry (file+headline "~/Dropbox/org/capture.org" "Code")
		   "* %^{Title} \nSOURCE: %:link\nCAPTURED: %U\n\n#+BEGIN_SRC\n%i\n#+END_SRC\n" :prepend t)
		  ("L" " Link capture" entry (file+headline "~/Dropbox/org/capture.org" "Link")
		   "* [[%:link][%:description]] \nCAPTURED: %U\nREMARKS: %?" :prepend t)))
  (setq org-refile-targets
		'(("~/Dropbox/org/archives.org" :level . 1)
		  ("~/Dropbox/org/remember.org" :level . 1)
		  ("~/Dropbox/org/task.org" :level . 1)))
  :init
  ;; Maximize the org-capture buffer
  (defvar my:org-capture-before-config nil
    "Window configuration before 'org-capture'.")
  (defadvice org-capture (before save-config activate)
    "Save the window configuration before 'org-capture'."
    (setq my:org-capture-before-config (current-window-configuration)))
  (add-hook 'org-capture-mode-hook 'delete-other-windows))
```

### 10.3 open-junk-file
junkファイルの保存も howmフォルダーに置くことで、howmの検索機能が利用できて便利です。

```emacs-lisp
(leaf open-junk-file :ensure t
  :config
  (setq open-junk-file-format "~/Dropbox/howm/junk/%Y%m%d.")
  (setq open-junk-file-find-file-function 'find-file))
```

下記のTipsを参考にして、直近の junkファイルを即開けるように `open-last-junk-file` を定義しました。

* [`Emacs で作成した使い捨てファイルを簡単に開く`](htotps://qiita.com/zonkyy/items/eba6bc64f66d278f0032) 

```elisp
(leaf em-glob
 :require t
 :config
 (defvar junk-file-dir "~/Dropbox/howm/junk/")
 (defun open-last-junk-file ()
   "Open last created junk-file."
   (interactive)
   (find-file
    (car
	    (last (eshell-extended-glob
	   	   (concat
   			(file-name-as-directory junk-file-dir)
			"*.*.*")))))))
```

### 10.4 toggle scratch
なんだかんだで便利な `scratch` なので `toggle-scratch` を設定して愛用しています。

編集中のバッファーとscratchバッファーとをToggle表示します。

```elisp
(defun toggle-scratch ()
 "Toggle current buffer and *scratch* buffer."
 (interactive)
 (if (not (string= "*scratch*" (buffer-name)))
         (progn
		 (setq toggle-scratch-prev-buffer (buffer-name))
		 (switch-to-buffer "*scratch*"))
	 (switch-to-buffer toggle-scratch-prev-buffer)))
```

## 11. σ(`ε´) ｵﾚ 流 Function Key 設定

かなり我流かつ邪道的なキーバインドなので参考にはならないかと…。

### F1: emacs help
Emacs標準の helpキーなのでそのまま使います。

which-key.el を導入することで各コマンドのガイドがミニバファーに表示されるので便利です。

![Alt Text](https://live.staticflickr.com/65535/51419241903_d99af2153c_b.jpg) 

```emacs-lisp
(leaf which-key
  :ensure t
  :config
  (which-key-mode 1)
  (setq which-key-max-description-length 40)
  (setq which-key-use-C-h-commands t))
```

### F2: imenu-list-smart-toggle

`counsel-css` と併用することで cssファイル編集時に重宝しています。

![Alt Text](https://live.staticflickr.com/65535/51419973025_01d97fe83b_b.jpg) 

```emacs-lisp
(leaf imenu-list
  :ensure t
  :bind ("<f2>" . imenu-list-smart-toggle)
  :config
  (setq imenu-list-size 30)
  (setq imenu-list-position 'left)
  (setq imenu-list-focus-after-activation t))

(leaf counsel-css
  :ensure t
  :config
  (add-hook 'css-mode-hook #'counsel-css-imenu-setup))
```


### F3: filer-current-dir-open
編集中のフィル（または dired）のカレントデレクトリで debianのファルマネージャー `nautilus` を開きます。

```emacs-lisp
(defun filer-current-dir-open ()
  "Open filer in current dir."
  (interactive)
  (shell-command (concat "nautilus " default-directory)))
(bind-key "<f3>" 'filer-current-dir-open)
```

### F4: term-current-dir-open
編集中のフィル（または dired）のカレントデレクトリで debianのターミナル `gonome-terminal` を開きます。

```emacs-lisp
(defun term-current-dir-open ()
  "Open terminal application in current dir."
  (interactive)
  (let ((dir (directory-file-name default-directory)))
	(shell-command (concat "gnome-terminal --working-directory " dir))))
(bind-key "<f4>" 'term-current-dir-open)
```
`eshell` も併用していて、同様に自動的にバッファーファイルのカレントで開くようにしています。

```emacs-lisp
(defun eshell-on-current-buffer ()
  "Set the eshell directory to the current buffer."
  (interactive)
  (let ((path (file-name-directory (or  (buffer-file-name) default-directory))))
	(with-current-buffer "*eshell*"
	  (cd path)
	  (eshell-emit-prompt))))
```


### F5: quickrun
open-junk-fileで作成したショートコードをバッファーで開いた状態で、即試運転できるすぐれものです。

```emacs-lisp
(leaf quickrun
  :ensure t
  :bind ("<f5>" . quickrun))
```
### F6: select-counsel-command

counselのキーバンドはあまりに多すぎて覚えられません。

`Mx counsel "^counsel "` することでミニバファーに表示される一覧から選ぶと便利です。

![Alt Text](https://live.staticflickr.com/65535/51418255432_9a79b6db79_b.jpg) 

```emacs-lisp
(defun select-counsel-command ()
  "Narrow the only counsel-command in `M-x'."
  (interactive)
  (counsel-M-x "^counsel "))
  (bind-key "<f6>" 'select-counsel-command)
```

### F7: calendar
作業中にカレンダーをチラ見したいときもあります。

Calfwまでは必要ないので標準機能の calendarを使っています。F7を押すことで、表示/非表示をトグルします。
`japanese-holidays` を installし [土日、今日] を強調させています。

![Alt Text](https://live.staticflickr.com/65535/48659029836_932b26293e_b.jpg) 

```emacs-lisp
(eval-after-load "calendar"
  (leaf japanese-holidays
	:ensure t :require t
	:bind (("<f7>" . calendar)
		   (:calendar-mode-map
			("<f7>" . quit-window)))
	:config
	(setq calendar-holidays
		  (append japanese-holidays holiday-local-holidays holiday-other-holidays))
	(setq calendar-mark-holidays-flag t) ; Show holidays on calendar
	(setq japanese-holiday-weekend '(0 6)     ; Show Saturdays and Sundays as holidays
		  japanese-holiday-weekend-marker     ; Saturday is displayed in light blue
		  '(holiday nil nil nil nil nil japanese-holiday-saturday))
	(add-hook 'calendar-today-visible-hook 'japanese-holiday-mark-weekend)
	(add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)
	(add-hook 'calendar-today-visible-hook 'calendar-mark-today)))
```

### F8: google-translate

Emacsの google-translateがときどき動かなくなることが多くなりました。

やむなくプラウザに出力する `google-translate-region` という関数や WEBの翻訳ページを開くための `google-translate-web`  という簡単な関数を作りました。これらのコマンドをミニバッファーの一覧から選べるように `F8` に割り当てました。

![Alt Text](https://live.staticflickr.com/65535/51424994862_f260aa163f_b.jpg) 


```emacs-lisp
(leaf google-translate
  :ensure t
  :bind (("C-t" . google-translate-auto)
		 ("<f8>" . select-google-translate))
  :config
  (defun google-translate-auto ()
	"Automatically recognize and translate Japanese and English."
	(interactive)
	(if (use-region-p)
		(let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
		  (deactivate-mark)
		  (if (string-match (format "\\`[%s]+\\'" "[:ascii:]")
							string)
			  (google-translate-translate
			   "en" "ja"
			   string)
			(google-translate-translate
			 "ja" "en"
			 string)))
	  (let ((string (read-string "Google Translate: ")))
		(if (string-match
			 (format "\\`[%s]+\\'" "[:ascii:]")
			 string)
			(google-translate-translate
			 "en" "ja"
			 string)
		  (google-translate-translate
		   "ja" "en"
		   string)))))

  ;; Fix error of "Failed to search TKK"
  (defun google-translate--get-b-d1 ()
  	"Search TKK."
  	(list 427110 1469889687))

  (defun google-translate-region ()
	"Open google translate with chromium."
	(interactive)
	(if (use-region-p)
		(let ((string (buffer-substring-no-properties (region-beginning) (region-end))))
		  (deactivate-mark)
		  (if (string-match (format "\\`[%s]+\\'" "[:ascii:]")
							string)
			  (browse-url (concat "https://translate.google.com/?source=gtx#en/ja/"
								  (url-hexify-string string)))
			(browse-url (concat "https://translate.google.com/?source=gtx#ja/en/"
								(url-hexify-string string)))))
	  (let ((string (read-string "Google Translate: ")))
		(if (string-match
			 (format "\\`[%s]+\\'" "[:ascii:]")
			 string)
			(browse-url
			 (concat "https://translate.google.com/?source=gtx#en/ja/" (url-hexify-string string)))
		  (browse-url
		   (concat "https://translate.google.com/?source=gtx#ja/en/" (url-hexify-string string)))))))

  (defun google-translate-web ()
	"Open Google-chrome for translate page."
	(interactive)
	(browse-url "https://translate.google.co.jp/?hl=ja"))

  (defun select-google-translate ()
	"Select google translat command."
	(interactive)
	(counsel-M-x "google-translate ")))
```

### F9: display-line-numbers-mode
行番号表示をトグルします。

```emacs-lisp
(leaf display-line-numbers
  :bind ("<f9>" . display-line-numbers-mode)
  :hook ((prog-mode-hook text-mode-hook) . display-line-numbers-mode))
```
### F10: neotree-toggle

neotreeの表示をトグルします。

```emacs-lisp
(leaf neotree
  :ensure t
  :bind (("<f10>" . neotree-find)
		 (:neotree-mode-map
		  ("RET" . neotree-enter-hide)
		  ("a" . neotree-hidden-file-toggle)
		  ("<left>" . neotree-select-up-node)
		  ("<right>" . neotree-change-root)
		  ("<f10>" . neotree-toggle)))
  :init
  (setq-default neo-keymap-style 'concise)
  (setq neo-create-file-auto-open t)
  :config
  (doom-themes-neotree-config))
```
### F11: toggle-frame-fullscreen
Emacsの標準機能なので、そのまま使います。

普段あまり使いませんが、解像度の小さいサブ機（Thinkpad X250）で作業するときなどには有用です。

### F12: my:darkroom-toggle

もっともよく使う執筆モードで、画面表示が以下のように変わります。

* フルスクリーンモード
* 行番号表示を消す
* 行間を少し広くする（お好みで…）

![Alt Text](https://live.staticflickr.com/65535/51419995320_384abc0671_b.jpg) 

```emacs-lisp
(leaf darkroom
  :ensure t
  :bind ("<f12>" . my:darkroom-mode-in)
  :config
  (defun my:darkroom-mode-in ()
	(interactive)
	(toggle-frame-fullscreen)
	(display-line-numbers-mode 0)
	(setq line-spacing 0.4)
	(darkroom-mode 1)
	(bind-key "<f12>" 'my:darkroom-mode-out darkroom-mode-map))

  (defun my:darkroom-mode-out ()
	(interactive)
	(darkroom-mode 0)
	(setq line-spacing 0.1)
	(display-line-numbers-mode 1)
	(toggle-frame-fullscreen)))
```

## 12. ユーティリティー関数

### 13.1 Scratch バッファーを消さない

難しい関数を設定せずとも内蔵コマンドで簡単に実現できます。

```emacs-lisp
;; Set buffer that can not be killed
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
  (emacs-lock-mode 'kill))
```
### 13.2 Terminal を Emacsから呼び出す

Emacsで開いているbufferのcurrent-dirでgonome-terminalを開く設定です。

こちらを使うようになってからはeshellを使わななりました。

```emacs-lisp
(defun term-current-dir-open ()
  "Open terminal application in current dir."
  (interactive)
  (let ((dir (directory-file-name default-directory)))
    (compile (concat "gnome-terminal --working-directory " dir))))
(bind-key "<f4>" 'term-current-dir-open)
```

### 13.3 Thunar を Emacsから呼び出す

```emacs-lisp
(defun filer-current-dir-open ()
  "Open filer in current dir."
  (interactive)
  (compile (concat "Thunar " default-directory)))
(bind-key "<f3>" 'filer-current-dir-open)
```


## 13. おわりに

以上が私の init.el とその説明です。

私の Emacsは、Webページのメンテナンスがメインで、プログラムミング・エディタというよりは、「賢くて多機能なワープロ」という存在です。ありえない…ような邪道キーバインドや未熟な点も多々ありますが、諸先輩に学びながら育てていきたいと願っています。

<div style="flort:left">
&ensp;<a href="https://twitter.com/share" class="twitter-share-button" data-url="{{ .Permalink }}" data-via="minorugh" data-text="{{ .Params.Title }}" data-lang="jp" data-count="horizontal">Tweet</a><script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script>
</div>
<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr"> <a href="https://twitter.com/minorugh/status/839117944260997120"></a></blockquote>

