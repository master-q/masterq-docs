# install: apt-get install rabbit
# view: rabbit --use-druby --public-level=all hogehoge.rd
# wiimote: rabbit-wiictl.rb hogehoge.rd
# control: rabbit-command --next
# or: $ irb1.9.1 -r drb/drb
#     irb> rabbit = DRbObject.new_with_uri("druby://localhost:10101")
#     irb> rabbit.send(:toggle_index_mode)
# print: rabbit --print -o hogehoge.pdf hogehoge.rd

= わかった気になるgitit-0.8
: subtitle
   初心者Haskell勉強会
: author
   Kiwamu Okabe
: theme
   debian-and-tortoise

= ルーティング

== プロパティ
: background-image
   draw/draw1.png
: background-image-relative-width
   75
: background-image-relative-margin-top
   7

= monadplus


= ページ表示



= 認証


= gititプラグイン#1
  gitit/Network/Gitit/Plugins.hs

がプラグイン読み込みコード。((-動的コード読み込みの詳細は http://www.bluishcoder.co.nz/2008/11/dynamic-compilation-and-loading-of.html を参照-))

= gititプラグイン#2
  # enscript haskell
  loadPlugin :: FilePath -> IO Plugin
  loadPlugin pluginName = do
  --snip--
        pr <- findModule (mkModuleName "Prelude") Nothing
        i <- findModule (mkModuleName "Network.Gitit.Interface") Nothing
        m <- findModule (mkModuleName modName) Nothing
        setContext []
  #if MIN_VERSION_ghc(7,0,0)
          [(m, Nothing), (i, Nothing), (pr, Nothing)]
  #else
          [m, i, pr]
  #endif
        value <- compileExpr (modName ++ ".plugin :: Plugin")
        let value' = (unsafeCoerce value) :: Plugin
        return value'

= plugins#1
最近は plugins が良いらしい

== プロパティ
: background-image
   image/shelarcy_plugins.png
: background-image-relative-width
   80
: background-image-relative-margin-top
   10

= plugins#2
