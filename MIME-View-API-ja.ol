『SEMI 1.5 MIME-View API の手引』
by 守岡 知彦

* はじめに

この文書は利用者界面と SEMI MIME-View の界面を作る人や SEMI MIME-View
の method を書くハッカーのために、SEMI MIME View の仕組みを解説し、API 
の仕様を明示します。


* MIME message


** content-type

[構造体] mime-content-type

	Content-Type 欄の解析結果を収めるための構造体。

	[要素]

	primary-type	media-type の主型 (symbol).

	subtype		media-type の副型 (symbol).

	parameters	Content-Type 欄の parameter (連想 list).

	上記の要素は参照関数 `mime-content-type-要素名' で参照する。


[関数] make-mime-content-type (type subtype &optional parameters)

	content-type の生成子。


[関数] mime-content-type-parameter (content-type parameter)

	CONTENT-TYPE の PARAMETER の値を返す。


[関数] mime-parse-Content-Type (string)

	STRING を content-type として解析した結果を返す。


[関数] mime-read-Content-Type ()

	現在の buffer の Content-Type 欄を読み取り、解析した結果を返す。

	Content-Type 欄が存在しない場合は nil を返す。


[関数] mime-type/subtype-string (type &optional subtype)

	type と subtype から type/subtype 形式の文字列を返す。


** content-disposition

[構造体] mime-content-disposition

	Content-Type 欄の解析結果を収めるための構造体。

	[要素]

	disposition-type	disposition-type (symbol).

	parameters		Content-Disposition 欄の parameter
				(連想 list).

	上記の要素は参照関数 `mime-content-disposition-要素名' で参照
	する。


[関数] mime-content-disposition-parameter (content-disposition parameter)

	CONTENT-DISPOSITION の PARAMETER の値を返す。


[関数] mime-content-disposition-filename (content-disposition)

	CONTENT-DISPOSITION の filename の値を返す。



* Message の表現と表示に関する概説

Internet の電子書簡・ネットニュースなどの書面 (message) の表現形式は 
STD 11 に基づいています。STD 11 の書面本体 (message body) は行を唯一の
構造とする簡易文面 (plain text) であり、文字符号も us-ascii と定められ
ています。実際には、文字符号を us-ascii の代わりにその言語圏で用いられ
る文字符号とした『地域化された STD 11』書面も用いられてきましたが、こ
の場合も書面の文字符号は１つです。このため、利用者界面 (Message User
Agent) は、しばしば、byte 列 = us-ascii 文字列、ないしは、byte 列 = そ
の言語圏で用いる文字符号の文字列のように見倣してきました。


	           ┏━━━━━━━━━━━━━━┓
	           ┃          message	         ┃
                   ┃         	                 ┃
       ─────→┨			         ┃
          data     ┃			         ┃  display
MTA      stream    ┃			         ┠─────→ user
           of      ┃			         ┃
         message   ┃			         ┃
       ←─────┨			         ┃
	   	   ┃			         ┃
		   ┃			         ┃
		   ┗━━━━━━━━━━━━━━┛
			図: 非 MIME MUA の場合


しかしながら、MIME では書面は entity を単位とする木構造になり、また、
１つの書面で複数の文字符号を用いることができます。また、entity の内容
は文面や絵のような単純に表示可能なものだけでなく、音声や動画などの一定
時間再生されるようなものや特定のアプリケーションのデータやプログラムの
ソース、あるいは、ftp や mail service の利用法や URL といった形で表さ
れた外部参照などのさまざまなものが考えらます。このため、表示だけを考え
ていた STD 11 における利用者界面の単純な延長では MIME の全ての機能を扱
うことはできません。つまり、MIME の形式に合わせて復号するだけでは不十
分であり、利用者との対話的な再生処理を考慮する必要があります。MIME 書
面の形式は自動処理がしやすく設計されていますが、MIME 書面に含まれる内
容の中にはセキュリティー上の問題から自動処理をするべきでないものがあり、
こういったものの再生に関しては利用者の判断を仰ぐように設計されるべきで
しょう。結局、MIME 書面を扱うためには STD 11 および MIME の構文で記述
されたメッセージの情報交換用表現とその解釈結果である表示画面や再生等の
処理を区別して考える必要があります。また、利用者との対話的な再生処理が
必要です。


			         ┏━━━━━━━━━━┓
			         ┃      preview       ┃display
                                 ┃       layer        ┃  or
			         ┃  ┌──────┐  ┃playback
		   ┌………………╂…┼………    ─┼─╂────→
                   ：            ┃  │          ←┼─╂─────
	       ┏━┿━━━━━━┫  └──────┘  ┃
	       ┃  ：    ┌─┐  ┃  ┌──────┐  ┃
	       ┃  ：    │…┼…╂…┼………    ─┼─╂────→
	       ┃  ：  ┌┤  │  ┃  │          ←┼─╂─────
	       ┃  ：  ││  │  ┃  └──────┘  ┃           user
               ┃  ：  │└─┘  ┃  ┌──────┐  ┃
       ───→┨┌┼┐│┌─┐  ┃  │          ─┼─╂────→
         data  ┃│：│││…┼…╂…┼………    ←┼─╂─────
MTA     stream ┃│  ├┼┤  │  ┃  └──────┘  ┃
          of   ┃│  │││  │  ┃  ┌──────┐  ┃
        message┃└─┘│└─┘  ┃  │          ─┼─╂────→
       ←───┨      │┌─┐  ┃  │    ：    ←┼─╂─────
	       ┃      ││  │  ┃  └──┼───┘  ┃navigation
	       ┃      └┤  │  ┃        ：          ┃
	       ┃        │…┼…╂…………┘          ┃
	       ┃        └─┘  ┗┳━━━━━━━━━┛
	       ┃       raw        ┃
	       ┃       layer      ┃
	       ┗━━━━━━━━━┛
			    図: MIME MUA の場合


このため、SEMI MIME-View は１つの書面に対して、情報交換用表現を格納す
る mime-raw-buffer と表示用表現を格納する mime-preview-buffer の２つの 
buffer を用います。


* mime-raw-buffer

  `mime-raw-buffer' は情報交換用形式のままの書簡の内容が収められる 
buffer です。MIME 書面は entity を単位とする木構造ですが、この buffer
の中では書面を構成する entity はこの構造にしたがって管理されます。即ち、
書面全体を表す root entity を指す buffer 局所変数
`mime-raw-message-info' で前章で解説した entity 構造体を指すことにより、
木構造を管理します。


** API

[buffer 局所変数] mime-raw-message-info

	書面の構造に関する情報を収める。

	［形式］mime-entity 構造体


[buffer 局所変数] mime-preview-buffer

	対応する mime-preview-buffer を示す。


[buffer 局所変数] mime-raw-representation-type

	mime-raw-buffer の representation-type を表す。

	representation-type とは mime-raw-buffer がどういう形式で表現
	されているかを示すもので、`binary' は network 表現のままである
	ことを示し、`cooked' は message 全体が既に code 変換されている
	ことを示す。

	nil の場合、mime-raw-representation-type-alist から得られた値
	が用いられる。


[buffer 局所変数] mime-raw-representation-type-alist

	major-mode と representation-type の連想 list.

	この変数から得られる値よりも mime-raw-representation-type の値
	の方が優先される。


[関数] mime-raw-find-entity-from-node-id (ENTITY-NODE-ID
					  &optional MESSAGE-INFO)

	書面構造 MESSAGE-INFO において ENTITY-NODE-ID に対応する
	entity を返す。

	MESSAGE-INFO が省略された場合は `mime-raw-message-info' の値を
	用いる。


[関数] mime-raw-find-entity-from-number (ENTITY-NUMBER
					 &optional MESSAGE-INFO)

	書面構造 MESSAGE-INFO において ENTITY-NUMBER に対応する entity 
	を返す。

	MESSAGE-INFO が省略された場合は `mime-raw-message-info' の値を
	用いる。


[関数] mime-raw-find-entity-from-point (POINT &optional MESSAGE-INFO)

	書面構造 MESSAGE-INFO において POINT に対応する entity を返す。

	MESSAGE-INFO が省略された場合は `mime-raw-message-info' の値を
	用いる。

[関数] mime-raw-point-to-entity-node-id (POINT &optional MESSAGE-INFO)

	書面構造 MESSAGE-INFO において POINT に対応する node-id を返す。
       
	MESSAGE-INFO が省略された場合は `mime-raw-message-info' の値を
	用いる。


[関数] mime-raw-point-to-entity-number (POINT &optional MESSAGE-INFO)

	書面構造 MESSAGE-INFO において POINT に対応する entity-number
	を返す。
       
	MESSAGE-INFO が省略された場合は `mime-raw-message-info' の値を
	用いる。


[関数] mime-raw-flatten-message-info (&optional message-info)

	書面構造 MESSAGE-INFO に含まれる全ての entity の list を返す。
       
	MESSAGE-INFO が省略された場合は `mime-raw-message-info' の値を
	用いる。


* mime-preview-buffer

  `mime-raw-buffer' は書簡の情報交換用表現を加工して作成した表示用表現
を収めるための buffer です。画面表示でも entity は意味のある単位で、１
つの書面は複数の entity を単位に構成されますが、MIME 書面の entity を
単位とする木構造は必ずしも重要ではなく、それ以上に画面表示上での位置が
重要です。また、entity は最小単位ではなく、表示上の構成要素が存在しま
す。また、表示上の要請から、entity に関係の無い要素も存在するかも知れ
ません。

  SEMI では entity は buffer 上の領域に張り付けられた text 属性
`mime-view-entity' で表現され、mime-raw-buffer 中の対応する entity を
指します。また、entity 以外の構成要素も text 属性を用いて表現されます。
これらの構成要素は場合によっては利用者の操作に対してなにがしかの反応を
示すために method を呼び出すことができます。この詳細に関しては後述しま
す。


** API

[buffer 局所変数] mime-mother-buffer

	対応する親 buffer を示す。

	親 buffer とはこの mime-preview-buffer と mime-raw-buffer の組
	を作る元となった mime-preview-buffer のことである。

	例えば、message/partial 形式の書面の表示に対して操作を行うこと
	によって、結合された書面に対する mime-preview-buffer ができた
	時、結合されたものにとって、操作を行った message/partial 形式
	の書面が親 buffer に相当する。


[buffer 局所変数] mime-raw-buffer

	対応する mime-raw-buffer を示す。

	[注意] この変数は使わない方が良い。なぜなら、
	       mime-preview-buffer は複数の mime-raw-buffer に対応する
	       可能性があるからである。 


[buffer 局所変数] mime-preview-original-window-configuration

	mime-preview-buffer を作る前の window-configuration を収める。


[text-property] mime-view-entity

	現在位置に対応する entity 構造体を示す。


[関数] mime-preview-original-major-mode (&optional recursive)

	現在位置に対応する entity の表象が存在する buffer の
	major-mode を返す。

	RECURSIVE に non-nil が指定された場合、始祖の major-mode を返
	す。


* entity

  MIME 書面は entity を単位とする木構造です。entity 構造体は entity や
書面全体の情報を格納する構造体で、以下では単に entity と呼ぶことにしま
す。

  SEMI MIME-View は書面を情報交換用表現を格納する mime-raw-buffer と表
示用表現を格納する mime-preview-buffer の２つの buffer で表現します。
このため、entity はこの２つの buffer にまたがって表現されます。

  mime-raw-buffer では entity は message の構造を表現するのに用いられ、
entity 階層の根、即ち、message の entity 構造体の中の木構造として管理
されます。以下では、message の entity 構造体のことを message-info と呼
ぶことにします。

  message-info 中の各 entity は木の節に当たりますが、この木には深さと
同じ深さの中の順番に従って番号が付けられます。即ち、


		              ┌───┐
	        	      │  nil │
                              └─┬─┘
              ┌─────────┼─────────┐
            ┌┴┐              ┌┴┐		    ┌┴┐
            │０│              │１│		    │２│
            └┬┘              └┬┘		    └┬┘
              │        ┌────┼────┐	      │
	  ┌─┴─┐┌─┴─┐┌─┴─┐┌─┴─┐┌─┴─┐
	  │ ０.０││ １.０││ １.１││ １.２││ ２.０│
	  └───┘└───┘└───┘└───┘└───┘
		       図: entity の階層と節番号


のように深さ n の節には長さ n の整数列の節番号が振れます。これを
entity-number と呼びます。entity-number は S 式としては (1 2 3) のよう
な整数のリストとして表現されます。

  一方、MIME-View では entity の管理に、これと同様の node-id を用いま
す。node-id はちょうど entity-number を逆にしたリストで、entity-number
1.2.3 に対応する node-id は (3 2 1) です。

  entity-number や node-id を用いることで、mime-raw-message における木
構造中での entity の相対的な位置関係を扱うことができます。

  以上のように entity は mime-raw-buffer では木構造として管理されます
が、mime-preview-buffer では entity は表示画面に対応する領域として管理
され、全体としては列構造になります。実際には変数がある訳ではなく、
`mime-view-entity' という text-property で表現されます。

  entity は単一の buffer における管理や情報の表現に使われる一方、この
２つの buffer をつなぐ情報としても用いられます。


** API

[構造体] mime-entity

	entity に関する情報を収める構造体。

	[要素]

	buffer			entity が存在する buffer (buffer).

	node-id			message 全体を表す entity の階層における、
				この entity の節としての位置を表す id
				(整数の list).

	header-start		header の先頭位置 (point).

	header-end		header の末尾位置 (point).

	body-start		body の先頭位置 (point).

	body-end		body の末尾位置 (point).

	content-type		content-type 欄の情報 (content-type).

	content-disposition	content-disposition 欄の情報
				(content-type).

	encoding		entity の Content-Transfer-Encoding
				(文字列)

	children		entity に含まれる entity の list
				(entity 構造体 の list).

	上記の要素は参照関数 `mime-entity-要素名' で参照する。


	[疑似要素]

	また、過去との互換性のため、以下の要素名の参照関数も利用可能であ
	る。

	point-min	entity の先頭位置 (point).

	point-max	entity の末尾位置 (point).

	type/subtype	entity の type/subtype (文字列).

	media-type	entity の media-primary-type (symbol).

	media-subtype	entity の media-subtype	 (symbol).

	parameters	entity の Content-Type field の parameter
			(連想 list).


[関数] make-mime-entity (node-id point-min point-max
			 media-type media-subtype parameters encoding
			 children)

	entity の生成子。


[関数] mime-entity-number (ENTITY)

	ENTITY の entity-number を返す。


[関数] mime-entity-parent (ENTITY &optional MESSAGE-INFO)

	ENTITY の親の entity を返す。

	MESSAGE-INFO が省略された場合は ENTITY が存在する buffer にお
	ける `mime-raw-message-info' の値を用いる。

	MESSAGE-INFO が指定された場合、これを根と見倣す。


[関数] mime-root-entity-p (ENTITY)

	ENTITY が root-entity（即ち、message 全体）である場合に、非
	nil を返す。



* entity の解釈と再生の仕組み

STD 11 や MIME は基本的に書面の構文を定めるのみであり、書面をどのよう
に表示すべきであるとか entity をどのように再生したり処理したりすべきか
といったことを定めません。このため、利用者界面は書面の構文を解釈し、こ
うしたことを決める必要があります。

構造情報からその表示や再生・処理に関する振舞を定義する一番簡単な方法は
構造情報に対して１対１でこうしたことを決めてしまうことです。即ち、構造
に対して表示や再生・処理を予め定義しておくことです。しかし、これでは表
示や再生・処理に関して異なるモデルの実装を作ることができない、あるいは、
異なる実装間で情報交換を行うことが困難になります。Internet では異なる
実装間で正しく情報が交換できることが求められますから、こうしたことはで
きません。また、特定の実装がこうした仮定に基づいた形式を生成することは
混乱の元となります。よって、STD 11 や MIME は異なる表示・処理モデル・
見かけを持った複数の実装に対して中立的な形式を定めるように設計されてい
る訳です。

構造情報に対して、見かけを提供する枠組としては SGML, XML, HTML 等で用
いるスタイルシートという方法があります。これは構造に対する処理を形式的
に定義するための枠組の上で、見かけを定義するスタイルシートを定義し、用
いるスタイルシートを指定することで、構造に対して見かけを与えます。

MIME の再生処理に関しては mailcap という形式があります。これは
media-type/subtype 等の entity の構造・形式に関する情報に対して、表示
や印刷等の再生・処理の仕方を定義します。

これらは構造情報に対して形式的にその意味を与える枠組で、構造情報に対す
る意味を変えることを可能にします。しかしながら、構造と意味は１対１対応
であり、解釈の状況依存性が存在しません。Internet の書面には常に形式や
意味の揺れが生じています。これは絶えず新しいプロトコルが提案される一方、
古い実装も残り、また、『地域化された RFC 822』のような慣習的なものも存
在するからです。また、一般的で詳細な規定を定めた場合も簡便な実装が存在
し、規定を完全にサポートする実装よりも簡便な実装の方が多数を占めること
がしばしばです。一方、規定を用いて詳細に指定された情報は有効に利用した
いのが人情です。

一方、


* Preview の生成

** 表示条件

[変数] mime-preview-condition

	entity の表示に関する条件木。


** entity-button

[関数] mime-view-entity-button-visible-p (ENTITY)

	非 nil の場合、entity-button を表示することを表す。


[関数] mime-view-insert-entity-button (ENTITY SUBJECT)

	ENTITY の entity-button を挿入する。


** entity-header

  preview-situation の 'header field の値が 'visible である時、その 
entityの header が表示されます。


*** header-filter

*** cutter


** entity-body

  preview-situation の 'body-presentation-method field の値が 
'with-filter であるか関数である時、その entity の body が表示されます。


*** body-presentation-method

  body-presentation-method は body の見かけを生成する関数で、

       (entity preview-situation)

という界面を持っています。


*** body-filter

  preview-situation の 'body-presentation-method field の値が 
'with-filter の時は、filter を用いる body-presentation-method を用いる
ことを示しています。この時、preview-situation の 'body-filter field の
値で示される filter 関数で処理された結果が表示されます。

  この filter 関数の界面は

       (preview-situation)

であり、この関数が呼ばれる時、処理対象となる entity の内容予め buffer 
に挿入されており、また、その領域は narrow されています。


* Entity の再生・処理

MIME-View は利用者が再生操作を行った時に、実行環境に応じて適切な解釈を
行い、再生処理を行うための機構を提供します。


	                 ┏━━━━━━━━━┓
       mime-raw-buffer   ┃        	     ┃
   ┏━━━━━━━━━━┻┓      	     ┃
   ┃information of message┃      	     ┃
   ┃		           ┃       	     ┃
   ┃  ┌───┐ operation┃type┌───┐  ┃       user's 
   ┃  │entity├←────╂──┤entity├←╂─── operation
   ┃  └─┬─┘┌──┐  ┃    └───┘  ┃
   ┃	   │    │MUA │  ┃	             ┃
   ┃	   │    │type│  ┃		     ┃
   ┃	   │    └┬─┘  ┣━━━━━━━━┛
   ┗━━━┿━━━┿━━━┛mime-preview-buffer
    	   │	   │
Information│	   │
   of	   │	   │
  entity   │	   │
   ＋	   │	   │
 operation │	   │
  type     │  	   │
	   ↓      ↓	
         ／┷━━━┷＼
         ┃  draft   ┃
         ┃   of     ┃
         ┃  acting  ┃
         ┃ situation┃
         ＼━━━━━／
	       │
	       │search
	       ↓
     ／━━━━━━━━━━━＼        ／━━━━━＼
     ┃mime-acting-condition ┃───→┃ acting   ┃
     ＼━━━━━━━━━━━／        ┃situation ┃
              	                       ＼━━┯━━／
					     │
					     │call
					     ↓
				       ┏━━┷━━┓        playback
				       ┃processing┠───→  for
                                       ┃ method   ┃          user
				       ┗━━━━━┛
			  図: 再生の仕組み


[変数] mime-acting-condition

	entity の再生・処理に関する条件木。
