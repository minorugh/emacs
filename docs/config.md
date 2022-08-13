---
layout: default
title: Index
nav_exclude: true
sort: 1
---

# GNU Emacs configuration


## 1. はじめに
* [Leaf](https://github.com/conao3/leaf.el) に乗り換えたのを機に大幅に整理したので [jekyll-rtd-theme](https://jamstackthemes.dev/theme/jekyll-rtd-theme/) でまとめました。
* Emacsを含むすべての環境設定ファイルを `ditfiles` リポジトリに置いて Git管理し [GitHub](https://github.com/minorugh/dotfiles) に公開しています。
* 設定ファイルは、メイン機とサブ機（いづれも Tinkpad）の複数端末で共有できるようにしています。
* 本ドキュメントは、[@takaxp](https://twitter.com/takaxp) さんの [init.el](https://takaxp.github.io/init.html) の記事から多くを吸収した模倣版です。
* 執筆用途に特化してカスタマイズしていますので、コンセプトやキーバイドなどは極めて邪道思想になっています。
* Emacs設定ファイル本体も、[GitHub](https://github.com/minorugh/dotfiles/tree/master/.emacs.d) に公開しています。


八十路も近い老骨ながら、[@masasam](https://twitter.com/SolistWork) さん、[@takaxp](https://twitter.com/takaxp) さんのご指導を得て、盲目的なパッチワークから多少なりとも自力でカスタマイズできるまで進化できました。感謝！

ファイル配置のデレクトリ構成は以下のとおりです。
```
~/.emacs.d
│
├── el-get/
├── elpa/
├── info/
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
Emacs-27導入にあわせて `early-init.el` を設定しました。 手順は以下のとおり。

1. `early-init.el` の読み込み
2. `init.el` の読み込み
3. `inits/` のファイル群を読み込み （init-loader 使用）

init-loader を使うことの是非については諸説あるようですが、[多くの恩恵](http://emacs.rubikitch.com/init-loader/)は捨て難く私には必須ツールです。

### 2.1 early-init-el

設定ファイル読み込み初期に起動画面がチラチラ変化するのを抑制しています。

因果関係は不明ですが結果的に起動時間の短縮にも効果があるようです。

```emacs-lisp
;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; For slightly faster startup
(setq package-enable-at-startup nil)

;; Always load newest byte code
(setq load-prefer-newer t)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(push '(fullscreen . maximized) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

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


(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
```

### 2.2 init.el

設定ファイル群は、init-loaderで読み込むようにしています。

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

;; Package
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
					   ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
	(package-refresh-contents)
	(package-install 'leaf))

  (leaf leaf-keywords
	:ensure t
	:init
	(leaf hydra :ensure t)
	(leaf el-get :ensure t)
	:config
	(leaf-keywords-init)))


;; Load init files
(leaf init-loader
  :ensure t
  :custom `((custom-file . "~/.emacs.d/tmp/custom.el")
			(init-loader-show-log-after-init . 'error-only))
  :config
  (init-loader-load))


(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
```

### 2.3 mini-init.el

[mini-init.el](https://github.com/minorugh/dotfiles/blob/main/.emacs.d/mini-init.el) は、最小限の emacs を起動させるための設定です。

新しいパッケージや設定をテストしたり、エラー等で Emacsが起動しない場合に使用します。
シェルから `eq` と入力することで起動することがでます。

以下を `.zshrc` または `.bashrc` に書き込みます。

```shell
alias eq = 'emacs -q -l ~/.emacs.d/mini-init.el'
```

ファイルの PATH は、ご自分の環境に応じて修正が必要です。


### 2.4 Dashboard バッファーを再生させる

Emacs 起動時の初期画面には、`Dashboard` を表示させています。

![Dashboard by doom-themes](https://live.staticflickr.com/65535/51631946053_b9d848a357_b.jpg)

`open-Dashboard` でいつでも Dashboard画面を再表示させることができ、`quit-Dashboard` 直前に作業していたバッファー画面に戻ります。

```emacs-lisp
(defun open-dashboard ()
  "Open the *dashboard* buffer and jump to the first widget."
  (interactive)
  (if (length> (window-list-1)
               (if (and (fboundp 'treemacs-current-visibility)
                        (eq (treemacs-current-visibility) 'visible))
                   2
                 1))
      (setq dashboard-recover-layout-p t))
  (delete-other-windows)
  (dashboard-refresh-buffer)
  (dashboard-goto-recent-files))

(defun quit-dashboard ()
  "Quit dashboard window."
  (interactive)
  (quit-window t)
  (when (and dashboard-recover-layout-p
			 (bound-and-true-p winner-mode))
    (winner-undo)
    (setq dashboard-recover-layout-p nil)))
```

- [Dashboard の詳細設定](https://github.com/minorugh/dotfiles/blob/main/.emacs.d/inits/01_dashboard.el)は、ここを見て下さい。


## 3. コア設定
Emacs を操作して文書編集する上で必要な設定。

### 3.1 言語 / 文字コード

シンプルにこれだけです。

``` emacs-lisp
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
```
### 3.2 日本語入力

Debian11 にインストールした Emacs上で [emacs-mozc](https://packages.debian.org/ja/jessie/emacs-mozc) を使っています。

#### 3.2.1 Emacsのときはインライン XIMを無効にする

Emacsをソースからビルドするときに `--without-xim` しなかったので、インライン XIMでも日本語入力ができてしまいます。
特に使い分けする必要もなく紛らわしいので `.Xresources` で XIM無効化の設定をしました。

```bash
! ~/.Xresources
! Emacs XIMを無効化
Emacs*useXIM: false

```
#### 3.2.2 [mozc] 句読点では即確定させる
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

#### 3.2.3 [mozc] ユーザー辞書を複数端末で共有させる

複数環境で Mozcユーザー辞書を共有できるようにしたいと思考中です。

Mozc のファイル群を Dropbox に保存して、シンボリックを各端末に置くという方法で一応は動きますが、同時使用すると競合コピーが作られるので多少問題もあります。辞書の共有は、Google Drive がよいという情報もあるので、時間のあるときにゆっくり試してみます。


## 4. カーソル移動
カーソルの移動は、原則デフォルトで使っていますが、以下の挙動だけ変更しています。



| ウインドウ移動           | C-q       |
| バッファー先頭・末尾     | C-a / C-e |
| 編集点の移動             | C-x C-x   |


### 4.1 ウインドウの移動

私の場合、基本二分割以上の作業はしないので `C-q` だけで便利に使えるこの関数は宝物です。

最初の `C-q` でに分割になり、二度目以降は `C-q` を押すたびに Window 移動します。

```emacs-lisp
(defun other-window-or-split ()
  "If there is one window, open split window.
If there are two or more windows, it will go to another window."
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))
(bind-key "C-q" 'other-window-or-split)
```


### 4.2 [sequential-command.el] バッファー先頭・末尾

地味なながら一度使うと便利すぎて止められません。

Melpaから Installできますが、私は HKey氏の改良版を el-getで使っています。

```emacs-lisp
(leaf sequential-command
  :doc "https://bre.is/6Xu4fQs6"
  :el-get HKey/sequential-command
  :config
  (leaf sequential-command-config
	:hook (emacs-startup-hook . sequential-command-setup-keys)))
```

### 4.3 [expand-region]カーソル位置を起点に選択範囲を賢く広げる


`er/expand-region` を呼ぶと、カーソル位置を起点として前後に選択範囲を広げてくれます。

2回以上呼ぶとその回数だけ賢く選択範囲が広がりますが、2回目以降は設定したキーバインドの最後の一文字を連打すれば OKです。その場合、選択範囲を狭める時は - を押し， 0 を押せばリセットされます。

```emacs-lisp

(leaf expand-region
  :ensure t
  :bind ("C-@" . er/expand-region))
```

## 5. 編集サポート
### 5.1 [selected] リージョン選択時のアクションを制御

選択領域に対するスピードコマンドです。

Emacsバッファーで領域を選択した後、バインドしたワンキーを入力するとコマンドが実行されます。
コマンドの数が増えてきたら、ヘルプ代わりに使える [counsel-selected](https://github.com/takaxp/counsel-selected) も便利そうです。

```emacs-lisp
(leaf selected
  :ensure t
  :global-minor-mode selected-global-mode
  :config
  (bind-key ";" 'comment-dwim selected-keymap)
  (bind-key "d" 'clipboard-kill-region selected-keymap)
  (bind-key "f" 'describe-function selected-keymap)
  (bind-key "v" 'describe-variable selected-keymap)
  (bind-key "c" 'clipboard-kill-ring-save selected-keymap)
  (bind-key "i" 'iedit-mode selected-keymap)
  (bind-key "s" 'swiper-thing-at-point selected-keymap)
  (bind-key "k" 'my:koujien selected-keymap)
  (bind-key "e" 'my:eijiro selected-keymap)
  (bind-key "w" 'my:weblio selected-keymap)
  (bind-key "t" 'google-translate-auto selected-keymap)
  (bind-key "g" 'my:google selected-keymap))
```

検索結果を browse-url で表示させるユーザーコマンドは、検索 urlのフォーマットとさえわかれば、パッケージツールに頼らずともお好みのマイコマンドを作成できます。

以下は、google と Weblio串刺し検索の例です。

```emacs-lisp
(leaf cus-selected
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
	   (unless (null my:ime-flag) (my:ime-on))))

  ;; User-functions-for-selected
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
	(buffer-substring-no-properties (region-beginning) (region-end))))
```

### 5.2 [selected] IME のオン・オフを自動制御する

selected.el は、うっかり IMEオンのまま選択領域に対するコマンドを選択すると、押下キーがバッファにそのまま入力されてしまいます。

領域を選択し始める時に IMEをオフにして、コマンド発行後に IMEを元に戻すという例が、
[@takaxp](https://qiita.com/takaxp) さんの [Qiitaの記事](https://qiita.com/takaxp/items/00245794d46c3a5fcaa8) にあったので、私の環境（emacs-mozc ）にあうように設定したら、すんなり動いてくれました。感謝！

```emacs-lisp
(defun my-activate-selected ()
  (selected-global-mode 1)
  (selected--on) ;; must call expclitly here
  (remove-hook 'activate-mark-hook #'my-activate-selected))
(add-hook 'activate-mark-hook #'my-activate-selected)

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
	 (unless (null my:ime-flag) (my:ime-on))))
```


### 5.3 [darkroom-mode] 執筆モード
[darkroom.el](https://github.com/joaotavora/darkroom)  は、画面の余計な項目を最小限にして、文章の執筆に集中できるようにするパッケージです。

タイトルバーやモードラインが一時的に削除されてフルスクリーンになり、テキストが拡大され、テキストがウィンドウの中央に配置されるように余白が調整されます。設定例では、さらに行番号表示を消し、行間を少し大きくしてより読みやすくしています。また、執筆モードで色修飾があるとかえって落ち着かないので強制的に text-modeにしています。

[F12] キーで IN/OUT をトグルしています。
darkroom-modeからでるとき text-modeに変える前のもとも major-modeに戻す必要が有るのですが、major-mode関数を操作する設定はすリスクが高そうだったので、一度 kill-bufferして再度読み込むようにしました。

```emacs-lisp
(leaf darkroom
  :ensure t
  :bind ("<f12>" . my:darkroom-in)
  :config
  (defun my:darkroom-in ()
	"Enter the darkroom-mode to always text-mode."
	(interactive)
	(text-mode)
	(display-line-numbers-mode 0)
	(setq line-spacing 0.4)
	(darkroom-tentative-mode 1)
	(bind-key "<f12>" 'my:darkroom-out darkroom-mode-map))
  (defun my:darkroom-out ()
	"Leave the darkroom-mode."
	(interactive)
	(darkroom-tentative-mode 0)
	(setq line-spacing 0.1)
	(display-line-numbers-mode 1)
	(my:close-current-buffer)
	(my:open-last-closed-buffer))
  :init
  ;; Open last closed file
  ;; http://ergoemacs.org/emacs/elisp_close_buffer_open_last_closed.html
  (defvar my:recently-closed-buffers nil)
  (defun my:close-current-buffer ()
	"Close the current buffer."
	(interactive)
	(setq my:recently-closed-buffers
		  (cons (cons (buffer-name) (buffer-file-name)) my:recently-closed-buffers))
	(kill-buffer (current-buffer)))
  (defun my:open-last-closed-buffer ()
	"Open the last closed buffer."
	(interactive)
	(find-file (cdr (pop my:recently-closed-buffers)))))
```

### 5.4 [yatex] YaTexで Tex編集

ごく一般的な設定例ですが、参考になるとしたら [yatexprc](https://www.yatex.org/gitbucket/yuuji/yatex/blob/c45e2a0187b702c5e817bf3023816dde154f0de9/yatexprc.el) の `M-x YaTeX-lpr` を使って一気に PDF作成まで自動化している点でしょうか。

```emacs-lisp
(leaf yatex
  :ensure t
  :mode ("\\.tex\\'" . yatex-mode)
  :config
  (setq tex-command "platex")
  (setq dviprint-command-format "dvpd.sh %s")
  (setq YaTeX-kanji-code nil)
  (setq YaTeX-latex-message-code 'utf-8)
  (setq YaTeX-default-pop-window-height 15)
  (leaf yatexprc
	:bind (("M-c" . YaTeX-typeset-buffer)
		   ("M-l" . YaTeX-lpr))))
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
### 5.5 swiper を migemo 化してローマ字入力で日本語を検索

[avy-migemo-e.g.swiper.el](https://github.com/momomo5717/avy-migemo) を使って出来ていたのですが、２年ほど前から更新が止まってしまっていて動きません。

つい最近、avy-migemo を使わない swiper-migemoを GitHubで見つけたので試した処、機嫌よく動いてくれています。
Melpaにはアップされていないみたいなので el-get で取得しています。

```emacs-lisp
  (leaf swiper-migemo
	:el-get tam17aki/swiper-migemo
	:global-minor-mode t)
```
### 5.6 pinky 小指問題解消
Emacs Pinky 小指問題の解消が目的で作られたものが、[Sollst Work Blog](https://solist.work/blog/posts/emacs-pinky-hydra/) にあったのでパクりました。

```
ソースコードを読んでいる時は Ctrlキーを押さないで作業できるようにしましょう。
```
hydraを使って実現しています。hydraが発動するとそれ以降は全てのショートカットキーを奪ってカスタマイズできます。
以前、view-modeベースで vim like に使えるような設定も試していたのですが、こちらのほうが馴染みやすいです。

```emacs-lisp
(key-chord-define-global
 "::"
 (defhydra hydra-pinky
   (:color red :hint nil)
   "
  :_SPC_._b_._a_._e_  :_h_._l_._j_._k_  :_0_._1_._o_._x_  :_-__.__+_  _d_iff:_n_._p_  buffer:_[__:__]_  _f_ile  _s_wiper"
   ;; move page
   ("h" backward-char)
   ("j" next-line)
   ("k" previous-line)
   ("l" forward-char)
   ("SPC" scroll-up-command)
   ("f" scroll-up-command)
   ("b" scroll-down-command)
   ("<next>" scroll-up-command)
   ("<prior>" scroll-down-command)
   ("g" beginning-of-buffer)
   ("G" end-of-buffer)
   ;; move line
   ("<down>" next-line)
   ("<up>" previous-line)
   ("<right>" forward-char)
   ("<left>" backward-char)
   ("a" seq-home)
   ("e" seq-end)
   ;; ("j" goto-line)
   ("<down>" next-line)
   ("<up>" previous-line)
   ("<right>" forward-char)
   ("<left>" backward-char)
   ;; window
   ("+" text-scale-increase)
   ("-" text-scale-decrease)
   ("." (text-scale-set 0))
   ("0" delete-window)
   ("1" delete-other-windows)
   ("2" split-window-below)
   ("3" split-window-right)
   ("x" window-swap-states)
   ("o" other-window-or-split)
   ;; diff-hl
   ("d" vc-diff)
   ("n" diff-hl-next-hunk)
   ("p" diff-hl-previous-hunk)
   ;; buffer
   (":" counsel-switch-buffer)
   ("[" winner-undo)
   ("]" winner-redo)
   ;; Others
   ("f" counsel-find-file)
   ("s" swiper)))
```

## 6. 表示サポート

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

### 6.2 rainbow-mode

rainbow-mode.el は red, greenなどの色名や #aabbcc といったカラーコードから実際の色を表示するマイナーモードです。
常時表示しているとうざいとケースのあるので、必要なときだけ使えるようにしています。

```emacs-lisp
(leaf rainbow-mode
  :ensure t
  :bind ("C-c r" . rainbow-mode))
```
### 6.3 custom-set-face
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

## 7. Hydra

[hydra.el](https://github.com/abo-abo/hydra) は、連続して操作するときにプレフィクスキーをキャンセルさせるための elispです。

日本では、[smartrep.el](http://sheephead.homelinux.org/2011/12/19/6930/) が有名だったようですが、hydra.elも同様の機能を提供します。
私はおもに8種の hydra を設定しています。それぞれを呼び出すための相関図は下記のとおりです。

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
   └── hydra-pinky
```
### 7.1 [hydra-work-menu / hydra-quick-dired] コマンドメニュー 

[hydra-work-menu](https://github.com/minorugh/dotfiles/blob/main/.emacs.d/inits/10_hydra-menu.el) には、
日記や俳句関係のシリーズ記事の追加、編集など、毎日頻繁に開くワークスペースへのショートカットを設定しています。

![hydra-work-menu](https://live.staticflickr.com/65535/50175364331_9fcf3c6c86_b.jpg) 

[hydra-quick-menu](https://github.com/minorugh/dotfiles/blob/main/.emacs.d/inits/10_hydra-menu.el) の方には、
編集作業で頻繁に使うツール群のほか、my:dired を開くためのショートカットも設定しています。

![hydra-quick-menu](https://live.staticflickr.com/65535/50174826063_b4fa442b1e_b.jpg) 



この２つの hydra は、いわば私の秘書のような役割で、どちらからでも相互にトグルで呼び出せるようにしています。

### 7.2 [hydra-extention] その他の Hydra 設定。
hydra で工夫するといろんなコマンドのキーバインドを記憶する必要もなく GUI 感覚で操作できるので積極的に使っています。

Qitta に詳しい記事を書いています。

- [Hydraで Emacsのキーバインド問題を解消](https://qiita.com/minoruGH/items/3776090fba46b1f9c228)


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

### 8.3 [undo-tree]
`C-x -u` で `undo-tree-visualize` を呼ぶと tee の下に diff表示もしてくれるように設定しています。

```emacs-lisp
(leaf undo-tree
  :ensure t
  :hook ((prog-mode-hook text-mode-hook) . undo-tree-mode)
  :config
  (bind-key* "C-_" 'undo-tree-undo)
  (bind-key* "C-\\" 'undo-tree-undo)
  (bind-key* "C-/" 'undo-tree-redo)
  (bind-key* "C-x u" 'undo-tree-visualize)
  :init
  (make-variable-buffer-local 'undo-tree-visualizer-diff)
  (setq-default undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-enable-undo-in-region nil)
  (setq undo-tree-auto-save-history nil)
  (setq undo-tree-history-directory-alist
		`(("." . ,(concat user-emacs-directory "undo-tree-hist/")))))
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

### 10.1 Org Mode / Howm Mode
私の場合、専門的なプログラミングもやらないし、現役引退の身なので GTDとかも必要ありません。Emacsはメモ書きに特化した使い方です。メモは全て howmを使って markdown記法で書いています。メモリストでカテゴリを分かりやすくするためのタグ付けをするのでメニュー機能として org-captureを使います。

captureでメモ機能を発動させると画面が半分になるのがいやなので、最大化で開くようにしました。また、dashboard画面に簡単なタスクを表示するに org-agendaも使ーっています。

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

(leaf org
  :config
  (bind-key "C-c a" 'org-agenda)
  (bind-key "C-c c" 'org-capture)
  (setq org-log-done 'time)
  (setq org-use-speed-commands t)
  (setq org-src-fontify-natively t)
  (setq org-agenda-files '("~/Dropbox/howm/org/task.org"))
  (setq org-capture-templates
		'(("t" " Task" entry (file+headline "~/Dropbox/howm/org/task.org" "Task")
		   "** TODO %?\n SCHEDULED: %^t \n" :prepend t)
		  ("m" " Memo" plain (file my:howm-create-file)
		   "# memo: %?\n%U %i")
		  ("n" " Note" plain (file my:howm-create-file)
		   "# note: %?\n%U %i")
		  ("p" "★ Perl" plain (file my:howm-create-file)
		   "# Perl: %?\n%U %i\n\n>>>\n\n```perl\n%i\n```")
		  ("e" "★ Emacs" plain (file my:howm-create-file)
		   "# emacs: %?\n%U %i\n\n```emacs-lisp\n%i\n```")
		  ("l" "★ Linux" plain (file my:howm-create-file)
		   "# linux: %?\n%U %i")))
  :init
  ;; Maximize the org-capture buffer
  (defvar my:org-capture-before-config nil
    "Window configuration before 'org-capture'.")
  (defadvice org-capture (before save-config activate)
    "Save the window configuration before 'org-capture'."
    (setq my:org-capture-before-config (current-window-configuration)))
  (add-hook 'org-capture-mode-hook 'delete-other-windows))
```

### 10.2 open-junk-file
junkファイルの保存も howmフォルダーに置くことで、howmの検索機能が利用できて便利です。また、直近の junkファイルを即開けるように open-last-junk-file を定義しました。

```emacs-lisp
(leaf open-junk-file :ensure t
  :config
  (setq open-junk-file-format "~/Dropbox/howm/junk/%Y%m%d.")
  (setq open-junk-file-find-file-function 'find-file)
  :init
  ;; https://qiita.com/zonkyy/items/eba6bc64f66d278f0032
  (leaf em-glob	:require t
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
				"*.*.*"))))))))
```

### 10.3 scratch buffer
メールや WEBなど全てを Emacsでという思想もありますが、窮屈なので私好みではありません。少し長めのメール文章などは、scratch バッファーに書いてコピペします。また、scratchを付箋代わりにも使うので、自動保存のために `persistent-scratch.el` を使います。

scratchバッファーの永続化にはいろいろ Tipsもあるようですが、標準機能で簡単に設定できます。なんだかんだで便利な scratchなので toggle-scratch を設定して愛用しています。

```emacs-lisp
;; Set buffer that can not be killed
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
  (emacs-lock-mode 'kill))

(leaf persistent-scratch
  :ensure t
  :config
  (persistent-scratch-setup-default))

(bind-key
 [S-return]
 (defun toggle-scratch ()
   "Toggle current buffer and *scratch* buffer."
   (interactive)
   (if (not (string= "*scratch*" (buffer-name)))
	   (progn
		 (setq toggle-scratch-prev-buffer (buffer-name))
		 (switch-to-buffer "*scratch*"))
	 (switch-to-buffer toggle-scratch-prev-buffer))))

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


## 12. フォント / 配色関係

### 12.1 フォント設定

メイン機（Thinkpad E590）とサブ機（Thinkpad X250）とで解像度が違うので設定を変えます。

```emacs-lisp
(add-to-list 'default-frame-alist '(font . "Cica-18"))
;; for sub-machine
(when (string-match "x250" (shell-command-to-string "uname -n"))
  (add-to-list 'default-frame-alist '(font . "Cica-15")))
```
### 11.2 [volatile-highlight]コピペした領域を強調
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

## 13. ユーティリティー関数

### 6.1 [emacs-lock-mode] scratch バッファーを消さない

難しい関数を設定せずとも内蔵コマンドで簡単に実現できます。

```emacs-lisp
;; Set buffer that can not be killed
(with-current-buffer "*scratch*"
  (emacs-lock-mode 'kill))
(with-current-buffer "*Messages*"
  (emacs-lock-mode 'kill))
```
### 13.1 Terminal を Emacsから呼び出す

```emacs-lisp
(defun term-current-dir-open ()
  "Open terminal application in current dir."
  (interactive)
  (let ((dir (directory-file-name default-directory)))
    (compile (concat "gnome-terminal --working-directory " dir))))
(bind-key "<f4>" 'term-current-dir-open)
```

### 13.2 Thunar を Emacsから呼び出す

```emacs-lisp
(defun filer-current-dir-open ()
  "Open filer in current dir."
  (interactive)
  (compile (concat "Thunar " default-directory)))
(bind-key "<f3>" 'filer-current-dir-open)
```

### 13.3 [restart-emacs] Emacsを再起動する
`C-x C-c` は、デフォルトで `(save-buffers-kill-emacs)` に割り当てられていますが、Emacsの再起動にリバインドしました。

```emacs-lisp
(leaf restart-emacs :ensure t
  :bind (("C-x C-c" . restart-emacs)))
```

## 14. おわりに

以上が私の init.el とその説明です。

私の Emacsは、Webページのメンテナンスがメインで、プログラムミング・エディタというよりは、「賢くて多機能なワープロ」という存在です。ありえない…ような邪道キーバインドや未熟な点も多々ありますが、諸先輩に学びながら育てていきたいと願っています。

<div style="flort:left">
&ensp;<a href="https://twitter.com/share" class="twitter-share-button" data-url="{{ .Permalink }}" data-via="minorugh" data-text="{{ .Params.Title }}" data-lang="jp" data-count="horizontal">Tweet</a><script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script>
</div>
<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr"> <a href="https://twitter.com/minorugh/status/839117944260997120"></a></blockquote>

