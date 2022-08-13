---
layout: default
title: Dotfiles
nav_order: 2
---

# Dotfiles

dotfilesとは、ホームディレクトリに置いてあるドット 
[.] から始まる設定ファイル（.bashrcとか）を管理しているリポジトリのことで、環境再構築に必要なシェルやエディタの設定からアプリケーションの設定まで幅広いものが置かれています。

<a href="https://solist.work/blog/posts/dotfiles/" class="btn"><i class="fa fa-file-text-o"></i> Dotfilesでバックアップ不要な開発環境を構築する</a>

## 1. dotfiles とバックアップ

dotfilesは、Gitなどでバージョン管理されていて GitHubで公開されていることが多いです。

dotfilesを極めることで、開発環境（パソコン）の更新や OSをクリーンインストールしたときなどでも自分の環境を瞬時に再構築できるという仕組みで、まさにこれからの時代の働き方だといえます。GitHubを検索すると多くの事例が見つかるので、ご自分の環境 (Windows / macOS / Linux) に対応したものを探して参考にされるといいです。

私の環境は、Thinkpad (PC) / Debian (OS) / Emacs (Editor) という構成です。

<a href="https://github.com/minorugh/dotfiles" class="btn" style="padding:1em; color:#333; font-size:150%"><i style="font-size:130%" class="fa fa-github"></i>  minorugh / dotfiles　<i style="font-size:110%;" class="fa fa-external-link"></i><a/>

## 2. Dropbox の活用

dotfiles のうち GitHubに配置できないものは、Dropboxに置くようにしています。

* ssh公開鍵などの機密情報
* 更新が頻繁で Git管理するのは面倒なもの（zsh_history,mozcの履歴やユーザー辞書など） 
* 公開できないバックアップデータ等

斯くして Dropboxに配置したファイルと GitHubと同期するGitリポジトリのファイルに関してはバックアップ不要になります。

## 3. 自動化のためのMakefile

敬愛する [石井 勝](http://objectclub.jp/community/memorial/homepage3.nifty.com/masarl/article/nmake.html) さんのWebページで下記の記述を見てとても感銘を受けました。

```
[優れた技術者になる条件]

1. 同じことは2度しない（Once and Only Once） 
2. 必ずしなければならない作業 → 自動化できないか考える 
```

<a href="http://objectclub.jp/community/memorial/homepage3.nifty.com/masarl/article/nmake.html" class="btn btn-default btn-lg">石井 勝/ 自動化のための nmake 入門</a>

dotfilesによる環境再構築を考える上で Makefileは必須のアイテムです。

## 4. Makefile の構成について

Makefileは，基本的に次のような構造になっています。

```makefile
[実行したい作業名]:
	[その作業を実行するためのコマンド行]
```
この一とかたまりをルールと呼びます。

ルールの中にある [実行したい作業名] のことをターゲットと呼びます。
[その作業を実行するためのコマンド行] の手前には、タブ1文字を入れることに注意してください。

例えば、"Hello World!" という文字が入った hello.txtというファイルを作る Makefileは次のようになります。

```makefile
hello.txt:
	echo Hello World > hello.txt
```

この makefileを配置したカレントデレクトリでターミナルから次のように入力することで、コマンド行が自動実行されるという仕組みです。

```shell
$ make hello.txt
```
makefileを本格的に学ぶとまだまだ奥が深いのですが、dotfilesで環境構築を自動化するには、この基本知識程度で十分なので "案ずるより産むが易し" 試行錯誤しながらステップアップして makefileを育ててください。

## 5. dotfiles を復元するための Makefale

私が実用している makefileは、[GitHub](https://github.com/minorugh/dotfiles/blob/main/Makefile) に置いてあります。

環境再構築の際の備忘録も兼ねているので、いろいろとコメントも書き加えてあります。
大まかには、次の３ステップで作業できるような構成になっています。

### 5.1 make実行前の準備作業
インストールメディアからＯＳ本体をクリーンインストールしたあと、makefileを実行して環境を復元させるのですが、その前に手作業で行うべきことをコメントでメモしておきます。

私の場合は、Linuxです。

```makefile
## =====================================================================
## Manual setting procedure
## =====================================================================

## 1. Boot from install-usb to install debian

## 2. Log in as root
## Register username to sudoers
# | gpasswd -a minoru sudo
# | log out
# | sudo visudo ## edit sudoers file to [%sudo  ALL=(ALL:ALL) NOPASSWD:ALL]
# | log out

## 3. Log in with ${USER}
## Make home directory English
# | sudo apt install -y xdg-user-dirs-gtk
# | LANG=C xdg-user-dirs-gtk-update --force
# | sudo apt update
# | sudo apt install -y zsh git nautilus chromium chromium-l10n
# | chsh -s /bin/zsh

## 4. Install dropbox & setting
# | sudo apt install -y nautilus-dropbox
# | Launch dropbox from Menu then install and setting

```
OSインストールのあと、sudoersと自動ログインの設定、デレクトリの日本語表記を英語に変更、基本アプリ（zsh git chromium）のインストール、dropboxのインストールまでを手作業で行い動作確認ができたら前準備完了です。

### 5.2 dotfileの復元とBase Pagegeのインストール
1st step としての作業を記述しています。

```shell
$ make init
```
という感じでターゲットごとに一つづつ確認しながら実行させてもいいですし、次のように記述しておくことで複数のターゲットをまとめて自動実行させることも可能です。

```makefile
allinstall: gnupg ssh base install init keyring tlp cica emacsmozc pipinstall
```

```shell
$ make allinstall
```

### 5.3 アプリ類のインストールと設定
時間のかかる texliveのインストールや apt でインスールできないアプリのインストールは、次のステップとして個別に makeを実行します。
OSバージョンとの整合などでエラーが生じて makeが止まってしまうこともあるので確認しながら進める必要があるからです。

## 6. おわりに

以上が私の dotfiles環境とその説明です。

Dotfilesでバックアップ不要な開発環境を完成させることで、昔ながらのバックアップの概念から解放されてシステムクラッシュなどに対する不安が一切不要になります。私自身プロのプログラマーではなく手習いレベルですので Tipsというよりはビギナーの備忘録に過ぎませんが、お役に立てれば嬉しいです。

<div style="flort:left">
&ensp;<a href="https://twitter.com/share" class="twitter-share-button" data-url="{{ .Permalink }}" data-via="minorugh" data-text="{{ .Params.Title }}" data-lang="jp" data-count="horizontal">Tweet</a><script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script>
</div>
<blockquote class="twitter-tweet" lang="ja"><p lang="ja" dir="ltr"> <a href="https://twitter.com/minorugh/status/839117944260997120"></a></blockquote>
