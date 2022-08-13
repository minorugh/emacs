---
layout: default
title: Gist
nav_order: 3
---

# Gist

[GitHub Gist](https://gist.github.com/minorugh) も利用しているのですが、Emacsらかの読み書き、修正が面倒なのでこちらへ移植中です。

---
## Gist Commands

2021.09.02

```shell
## 複数ファイル 
$ gist hoge.txt huga.txt
## ワイルドカード
$ gist *.txt

## ファイル名を別のものにして作成
$ gist -f gist_file_name local_file_name

## private gist作成
$ gist -p hoge.rb

## 説明文を加える
$ gist -d "説明文" hoge.rb

## 既存のgistを編集
$ gist -u gist_id file_name

## クリップボードにURLをコピー 
$ gist -c hoge.rb

## ブログなどにコードを乗せるときはこれ
## <script>タグで囲まれた、URLをコピー
$ gist -e hoge.rb

## gistを作成しつつ、ブラウザで確認(ブラウザ開きます)
$ gist -o hoge.rb

## gist削除
$ gist --delete gist_id

## gist一覧
$ gist -l

## あるgistの内容を表示
$ gist -r gist_id

```
---
## パーミッション早見表

2021.09.02

### パーミッション文字と8進数の対応

| 文字列 |８進数 |意味         |
|--------|-------|-------------|
| r      | 4     |読み込み可能 |
| w      | 2     |書き込み可能 |
| x      | 1     |実行権限     |
| -      | 0     |権限なし     |

### パーミッション文字列と8進数の対応

| 文字列 |８進数     |
|--------|-----------|
| rwx    | 7 = 4+2+1 |
| rw-    | 6 = 4+2+0 |
| r-x    | 5 = 4+0+1 |
| r--    | 4 = 4+0+0 |
| -wx    | 3 = 0+2+1 |
| -w-    | 2 = 0+2+0 |
| --x    | 1 =0+0+1  |
| ---    | 0         |

### 指定例

| 権限      |数値 |概要                       |
|-----------|-----|---------------------------|
| rw-r--r-- | 644 |オーナー以外は読み込み専用 |
| rw-rw-rw- | 666 |テキストファイルなど       |
| rwxr-xr-x | 755 |実行スクリプトなど         |


### chmodコマンド

| 変更対象 | 意味     |
|----------|----------|
| u        | ユーザー |
| g        | グループ |
| o        | その他   |
| a        | すべて   |

| 変更方法 | 意味                   |
|----------|------------------------|
| =        | 指定した権限にする     |
| +        | 指定した権限を付与する |
| -        | 指定した権限を除去する |

| 変更内容 | 意味     |
|----------|----------|
| r        | 読み取り |
| w        | 書き込み |
| x        | 実行権限 |

* 事項権限の付与→ `chmod +X`
 
---
## Install latest version of emacs

2021.08.28
```shell
cd ${HOME}/src
wget https://ftp.gnu.org/pub/gnu/emacs/emacs-27.2.tar.gz
tar -xzvf emacs-27.2.tar.gz
cd emacs-27.2
./configure
make
sudo make install
```

---
## Upftp SSL compatible version

2021.08.28

upftp.pl SSL対応版

```perl
#!/usr/bin/perl
#
# This is UpFtp, Version 1.3
# Copyright (C) 2000,2003,2006,2017 by Hiroshi Yuki.
# http://www.hyuki.com/upftp/
# https://gist.github.com/hyuki0000/f58ccabccba37b93dbb5823d4f019341
# This program is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.
# Add [my $ home = $ ENV {"HOME"}] to share on Mac & Linux at 20170713 by Minoru Yamada.
#

use strict;
use warnings;
use Net::FTP;
use Net::FTPSSL;

########################################
# Configuration
########################################
# FTPS: Do FTPS (FTP over SSL/TLS)?: 0 (no, use Net::FTP), 1 (yes, use Net::FTPSSL)
my $ftpssl = 1;
# FTPS: Encryption Mode.
my $ftpssl_encryption = EXP_CRYPT;
# Show debug info: 0 (nodebug), 1 (terse), 2 (verbose)
my $debug = 0;
# Show update info.
my $show_update = 1;
# Show ftp info: 0 (nodebug), 1 (print ftp session)
my $ftpdebug = 0;
# Do ftp?:  0 (yes, use ftp), 1 (no, simulation only)
my $noftp = 0;
# Your FTP host name.
my $hostname = 'sv2214.xserver.jp';
# Your FTP user name.
my $username = '******';
# Your FTP password (if empty, I ask you later)
my $password = '**********';
# Remote root directory (in fullpath)
my $remoterootdir = '/gospel-haiku.com/public_html';
# Get HOME directory from environment variables
my $home = $ENV{"HOME"};
# Local root directory (in fullpath)
my $localrootdir = "$home/Dropbox/Web/GH";
# File list (in fullpath)
my $filelist = "$home/Dropbox/Web/GH/upftp/filelist.txt";
# Binary file extension
my @binary_extension = qw(
    gif jpg png class pdf zip lzh tar gz tgz
);

########################################
# End of configuration.
########################################
my $ftp;
my @newfilelist;

&upftp;
exit(0);

sub upftp {
    if ($debug) {
        if ($noftp) {
            print "Simulation only, I do not transfer any file.\n";
        }
    }
    unless ($hostname) {
        print "\$hostname is empty, abort.\n";
        return;
    }
    unless ($username) {
        print "\$username is empty, abort.\n";
        return;
    }
    unless ($password) {
        print "Login to $hostname as $username\n";
        print "Password:";
        chomp($password = <STDIN>);
    }
    unless ($remoterootdir) {
        print "\$remoterootdir is empty, abort.\n";
        return;
    }
    unless ($localrootdir) {
        print "\$localrootdir is empty, abort.\n";
        return;
    }
    unless ($filelist) {
        print "\$filelist is empty, abort.\n";
        return;
    }
    print "filelist is $filelist\n" if ($debug);
    if (!open(FILELIST, $filelist)) {
        print "$filelist is not found.\n";
        return;
    }
    while (<FILELIST>) {
        chomp;
        next if (/^#/);
        my ($filename, $updatetime) = split(/,/);
        $updatetime = 0 if (not defined($updatetime) or $updatetime eq "");
        print "$filename = $updatetime\n" if ($debug > 1);
        my $mtime = (stat("$localrootdir/$filename"))[9];
        $mtime = 0 if (not defined($mtime) or $mtime eq "");
        print "mtime = $mtime\n" if ($debug > 1);
        if (defined($mtime) and $mtime > $updatetime) {
            print "Update $filename\n" if ($debug or $show_update);
            $filename =~ m|(.*)[/\\]|;
            my $subdir = defined($1) ? $1 : '';
            &ftp_put("$localrootdir/$filename", "$remoterootdir/$subdir");
        } else {
            print "Skip $filename\n" if ($debug);
        }
        my $curtime = time;
        push(@newfilelist, "$filename,$curtime\n");
    }
    close(FILELIST);
    &ftp_logout;
    if (!open(FILELIST, "> $filelist")) {
        print "$filelist: Cannot create.\n";
        exit(-1);
    }
    print FILELIST @newfilelist;
    close(FILELIST);
}

# Put $localfile to $remotedir.
sub ftp_put {
    my ($localfile, $remotedir) = @_;
    unless ($ftp) {
        unless ($noftp) {
            if ($ftpssl) {
                $ftp = Net::FTPSSL->new($hostname, Encryption => $ftpssl_encryption,  Debug => $ftpdebug);
            } else {
                $ftp = Net::FTP->new($hostname, Debug => $ftpdebug);
            }
            $ftp->login($username, $password);
        }
    }
    print "localfile = $localfile, remotedir = $remotedir\n" if ($debug > 1);
    unless ($noftp) {
        $ftp->cwd($remotedir);
        if (&is_binary($localfile)) {
            $ftp->binary();
        } else {
            $ftp->ascii();
        }
        $ftp->put($localfile);
    }
}

# Logout.
sub ftp_logout {
    if ($ftp) {
        unless ($noftp) {
            $ftp->quit;
        }
    }
}

# Ascii? Binary?
sub is_binary {
    my ($localfile) = @_;
    foreach my $ext (@binary_extension) {
        if ($localfile =~ /\.$ext$/) {
            return 1;
        }
    }
    return 0;
}
1;
```

```
{% raw %}{% gist c08ee0f2726fd0e3909d %}{% endraw %}
```

{% gist c08ee0f2726fd0e3909d %}
