open Kansai

(*駅名ペア、最短距離（実数）、駅名ペアのリストを３つをフィールドとして持つレコード型eki_t*)
type eki_t = {
  namae        :  string * string ;
  saitan_kyori :  float ;
  temae_list   :  (string * string)  list ;
}

(* 木と県名ペア×２と距離を渡されたら木のassociation listを更新した木を返す関数 *)
(* hittsuke :  ('a * 'b, (('c * 'd) * 'e) list) Tree.t ->
  'a -> 'b -> 'c -> 'd -> 'e -> ('a * 'b, (('c * 'd) * 'e) list) Tree.t *)
let hittsuke tree a ak b bk kyo =
  let lst = try Tree.search tree (a,ak) with Not_found -> [] in
  let nlst = ((b,bk),kyo) :: lst in
  Tree.insert tree (a,ak) nlst
                                                          

(* 問１で考えた型の木ekikan_treeとekikan_t 型の駅間を受け取ったらその情報を挿入した木を返す関数 *)
(* insert_ekikan :  (string * string, ((string * string) * float) list) Tree.t ->
  ekikan_t -> (string * string, ((string * string) * float) list) Tree.t *)
let insert_ekikan ekikan_tree ekikan = match ekikan with
 {kiten = kit; (* 起点 *)
  kenk = kitken; (* 起点の県名 *)
  shuten = shu; (* 終点 *)
  kens = kenshu; (* 終点の県名 *)
  keiyu = kei; (* 経由路線名 *)
  kyori = kyo;  (* 距離 *)
  jikan = jik;    (* 所要時間 *)
} -> hittsuke (hittsuke ekikan_tree kit kitken shu kenshu kyo) shu kenshu kit kitken kyo

(* 駅名ペア１と（駅名ペア,距離）のリストを受け取ったら、駅名ペア１に対応する距離を返す関数 *)
(* kyori : 'a * 'b -> (('a * 'b) * 'c) list -> 'c *)
let rec kyori (d,e) lst = match lst with
    [] -> infinity
  |((f,g),kyo)::rest -> if d = f && e = g then kyo
    else kyori (d,e) rest


(*　駅名ペアをふたつとekikan_treeを受け取って来たら、 その２駅（ペア）間の距離を返す関数　*)
(* ２駅がつながっていなかったらinfinityを返す *)
(*　get_ekikan_kyori2 :  'a * 'b -> 'c * 'd -> ('a * 'b, (('c * 'd) * float) list) Tree.t -> float　*)
                                                             
let rec get_ekikan_kyori2 (b,c) (d,e) ekikan_tree =
  let lst = try Tree.search ekikan_tree (b,c) with
      Not_found -> raise Not_found in
  kyori (d,e) lst

(*目的：ローマ字の文字列による駅名と駅名リストを受け取ったら、その駅の漢字表記と県名のペアを返す関数*)
(*romaji_to_kanji2 : string -> ekimei_t list -> string * string *)
            
let rec romaji_to_kanji2 a lst = match lst with
    [] -> ("","")
  |{
    kanji=cal; (* 漢字の駅名 *)
    kana=kan;(* 読み *)
    romaji=rom;(* ローマ字 *)
    ken=ke; (* 県名 *)
    shozoku=sho (* 所属路線名 *)
  }::rest -> if a = rom then (cal,ke)
    else romaji_to_kanji2 a rest

(*県名が異なる場合は、県名の文字列の「小さい方」が先に出てきて、県名が同じ場合は、漢字の文字列の「小さい方」が先に出てくる関数narabe*)
(*narabe : ekimei_t lst -> ekimei_t -> ekimei_t lst*)
let rec narabe lst {kanji=cal;kana=kan;romaji=rom;ken=ke;shozoku=sho;} = match lst with
    [] -> [{kanji=cal;kana=kan;romaji=rom;ken=ke;shozoku=sho;}]

  | {kanji=cal2;kana=kan2;romaji=rom2;ken=ke2;shozoku=sho2;}::rest
    -> if ke < ke2  then  {kanji=cal;kana=kan;romaji=rom;ken=ke;shozoku=sho;} :: lst

    else  if ke > ke2  then {kanji=cal2; kana=kan2;romaji=rom2;ken=ke2; shozoku=sho2;}::narabe rest {kanji=cal; kana=kan;romaji=rom;ken=ke; shozoku=sho;}

    else if cal < cal2  then  {kanji=cal;kana=kan;romaji=rom;ken=ke;shozoku=sho;}::lst
    else if cal > cal2  then {kanji=cal2; kana=kan2;romaji=rom2;ken=ke2; shozoku=sho2;}::narabe rest {kanji=cal;kana=kan;romaji=rom;ken=ke;shozoku=sho;}
    else {kanji=cal2;kana=kan2;romaji=rom2;ken=ke2;shozoku=sho2} :: rest

(*ekimei_t 型のリストを受け取ったら、それを上に述べた順に整列し、 さらに重複した駅を取り除いた ekimei_t 型のリストを返す関数*)
(* seiretsu2 : ekimei_t list -> ekimei_t list *)

let rec seiretsu2 lst = match lst with
    [] -> []
  | {kanji=cal; (* 漢字の駅名 *)
    kana=kan;(* 読み *)
    romaji=rom;(* ローマ字 *)
    ken=ke; (* 県名 *)
    shozoku=sho; (* 所属路線名 *)
    }::rest -> narabe (seiretsu2 rest) {kanji=cal;kana=kan;romaji=rom;ken=ke;shozoku=sho;}



(* ekimei_t 型のリストを受け取ったら、その駅名を使って eki_t 型のリストを作る関数 *)
(* make_eki_list2 : ekimei_t list -> eki_t list *)
let rec make_eki_list2 lst = match lst with
    [] -> []
  | {
  kanji   = cal; (* 漢字の駅名 *)
  kana    = kan; (* 読み *)
  romaji  = rom; (* ローマ字 *)
  ken     = ken; (* 県名 *)
  shozoku = sho; (* 所属路線名 *)
} :: rest -> {namae=(cal,ken);saitan_kyori=infinity;temae_list=[];} :: (make_eki_list2 rest)

(* eki_t 型のリストと起点の駅名ペアを受け取ったら、 起点のみ上記のようになっており、 起点以外はもとと同じであるような駅のリスト （要素が eki_t 型であるようなリスト） を返す関数 *)
(* shokika2 : eki_t list -> string * string -> eki_t list *)
let rec shokika2 lst (a,b) = match lst with
    [] -> []
  | {
  namae =nam;
  saitan_kyori = sai;
  temae_list = tem;
} :: rest -> if nam = (a,b) then {namae=nam;saitan_kyori=0.;temae_list=[nam;]}::rest
    else {namae =nam;saitan_kyori = sai;temae_list = tem;}::(shokika2 rest (a,b))


(* eki_t list 型のリストを受け取ったら、「最短距離最小の駅」と「最短距離最小の駅以外からなるリスト」の組 （(eki_t * eki_t list) 型）を返す関数 *)
(* saitan_wo_bunri2 : eki_t list -> eki_t * eki_t list *)

let rec saitan_wo_bunri2 lst = match lst with
    [] -> ({namae=("","");saitan_kyori=infinity;temae_list=[];},[])
  |[eki_t] -> (eki_t,[])
  | first :: rest -> 
    let (a,b) = saitan_wo_bunri2 rest in 
    match (first,a) with
      ({namae=fnam;saitan_kyori=fsai;temae_list=ftem},{namae=anam;saitan_kyori=asai;temae_list=atem}) ->
      if fsai < asai then (first , a::b)
    else (a,first::b)


(* （直前に最短距離を確定した）点 p（eki_t 型）と 最短距離が未確定の点 q（eki_t 型）、および 駅間の木を受け取ったら、 （第４回の課題で作った） get_ekikan_kyori2 を使って q が p に接続しているかを調べ、 接続していたら最短距離と手前リストが次のようになっている新しい q を 返す関数 koushin1。
１，現在、q が保持している最短距離と、p 経由で q に行った場合の距離（p の最短距離に pq 間の距離を加えたもの）を比べ、 新しい q の最短距離はそのうち小さい方とする。
２， p 経由で行った場合の方が短かった場合は、 p の手前リストの先頭に q（の駅名ペア）を加えたものを q の temae_list にする。
３，接続していなかったら q をそのまま返す。*)
(* koushin1 : eki_t ->
  eki_t ->
  (string * string, ((string * string) * float) list) Tree.t -> eki_t *)
let koushin1 {namae=(n1,n2);saitan_kyori=s1;temae_list=l1;} {namae=(n3,n4);saitan_kyori=s2;temae_list=l2;} ektree =
  let a = s1 +. get_ekikan_kyori2 (n1,n2) (n3,n4) ektree in
  if s2 > a then {namae=(n3,n4);saitan_kyori=a;temae_list=(n3,n4)::l1;}
  else {namae=(n3,n4);saitan_kyori=s2;temae_list=l2;}

(* （直前に最短距離を確定した）点 p（eki_t 型）と 最短距離が未確定の点の集合 V（eki_t list 型）、および 駅間の木を受け取ったら、 V 中の全ての駅について、必要に応じて更新処理を行った後の 未確定の駅の集合を返す関数 koushin *)
(* koushin : eki_t ->
  eki_t list ->
  (string * string, ((string * string) * float) list) Tree.t -> eki_t list *)
let rec koushin {namae=(n1,n2);saitan_kyori=s1;temae_list=l1;} lst1 ektree = match lst1 with
    [] -> []
  |{namae=(n3,n4);saitan_kyori=s2;temae_list=l2;} :: rest
    -> koushin1 {namae=(n1,n2);saitan_kyori=s1;temae_list=l1;} {namae=(n3,n4);saitan_kyori=s2;temae_list=l2;} ektree :: (koushin {namae=(n1,n2);saitan_kyori=s1;temae_list=l1;} rest ektree)

(* 
1,最短距離が未確定の点の集合 V が空になったら終了。 （自明なケース）
2,そうでない間は V から最短距離最小の点 p を選び（p を 確定し）、p に接続している点の最短距離を更新した後、p を除いた V について最短路問題を解く。（再帰によるケース）

起点のみ最短距離が0で他はinfinityとなっている駅のリスト（eki_t list 型）と駅間の木を受け取ったら、（上に述べた方針に従ってダイクストラ法を動かし、最終的に）「起点からの最短距離と『起点からその駅に至る 駅名の（逆順の）リスト』が入った駅」のリスト（eki_t list 型）を返すような関数 *)
(* dijkstra_main : eki_t list -> (string * string, ((string * string) * float) list) Tree.t -> eki_t list *)
let rec dijkstra_main lst1 ektree =
match lst1 with
    [] -> []
  | first::rest -> let (p,v) = saitan_wo_bunri2 lst1 in
    p::dijkstra_main (koushin p v ektree) ektree

(* eki_t型のリストと駅名ペアを受け取ったら、リストからその駅を見つけて返す関数 *)
(* ekimikke : eki_t list -> string * string -> eki_t *)
let rec ekimikke lst (a,k) = match lst with
    [] -> {namae=("","");saitan_kyori=infinity;temae_list=[];}
  | {namae=(n1,n2);saitan_kyori=sai;temae_list=tem;}::rest ->
    if n1=a && n2=k then {namae=(n1,n2);saitan_kyori=sai;temae_list=tem;}
    else ekimikke rest (a,k)

(* ekikan_listを受け取ってekikan_treeを作る関数 *)
(* maketree : ekikan_t list -> (string * string, ((string * string) * float) list) Tree.t *)
let rec maketree ekikanlst = match ekikanlst with
    [] -> Tree.empty
  |{kiten = kit; (* 起点 *)
  kenk = kitken; (* 起点の県名 *)
  shuten = shu; (* 終点 *)
  kens = kenshu; (* 終点の県名 *)
  keiyu = kei; (* 経由路線名 *)
  kyori = kyo;  (* 距離 *)
  jikan = jik;    (* 所要時間 *)
}::rest ->  insert_ekikan (maketree rest) {kiten = kit;kenk = kitken;shuten = shu;kens = kenshu;keiyu = kei;kyori = kyo;jikan = jik;}


(* 起点の（ローマ字の）駅名と終点の（ローマ字の）駅名と駅名リスト（ekimei_t list 型・駅間リスト（ekikan_t list 型を受け取ったら、
romaji_to_kanji2 を使って起点と終点の駅名ペアを求め、
受け取った駅名リストから seiretsu2 を使って重複を取り除き、
make_eki_list2 と shokika2 （または make_initial_eki_list2）を使って 駅のリスト（eki_t list 型）を作り、
dijkstra_main を使って、各駅までの最短路を確定し、
   その中から終点の駅（eki_t 型）を探して返すような関数 *)
(* dijkstra : string -> string -> ekimei_t list -> ekikan_t list -> eki_t *)
let dijkstra kit shu ekimei_list ekikan_list =
  let sei_ekimei_list = seiretsu2 ekimei_list in
  let (kitkan,kitken),(shukan,shuken) = (romaji_to_kanji2 kit sei_ekimei_list),(romaji_to_kanji2 shu sei_ekimei_list) in
  ekimikke (dijkstra_main (shokika2 (make_eki_list2 sei_ekimei_list) (kitkan,kitken)) (maketree ekikan_list)) (shukan,shuken)





(* 最短経路のリストを受け取って最短経路をプリントする関数 *)
(* keiro :  (string * string) list -> unit *)
let rec keiro lst =
  match lst with
    [] -> print_newline ()
  |(a,b)::rest ->
    (keiro rest;
     print_string a;
       print_string "(";
       print_string b;
       print_string ") -> ")
       
    
(* 最短路が求まったら最短経路と最短距離を「きれいに」出力する関数 *)
(* kirei : string -> string -> ekimei_t list -> ekikan_t list -> unit *)
let kirei kit shu ekimei_list ekikan_list =
  let {namae = (na,ke); saitan_kyori = kyo; temae_list = lst} = dijkstra kit shu ekimei_list ekikan_list in
  if kyo = infinity then
    (print_newline ();
     print_string kit;
     print_string "または";
     print_string shu;
     print_string "は存在しません。";
     print_newline ())
  else
    (print_newline ();
     print_string "最短距離は";
     print_float kyo;
     print_string "です。最短経路は";
     keiro lst;
     print_string "到着";
     print_newline ())
(*
let test1 = kirei "kawaramachi" "deyashiki" global_ekimei_list global_ekikan_list
let test2 = kirei "kisaragi" "deyashiki" global_ekimei_list global_ekikan_list
let test3 = kirei "hiratacho" "kintetsutomida" global_ekimei_list global_ekikan_list
*)

(* テスト結果 *)
(*
最短距離は164.です。最短経路は
川原町(三重) -> 近鉄四日市(三重) -> 新正(三重) -> 海山道(三重) -> 塩浜(三重) -> 北楠(三重) -> 楠(三重) -> 長太ノ浦(三重) -> 箕田(三重) -> 伊勢若松(三重) -> 千代崎(三重) -> 白子(三重) -> 鼓ヶ浦(三重) -> 磯山(三重) -> 千里(三重) -> 豊津上野(三重) -> 白塚(三重) -> 高田本山(三重) -> 江戸橋(三重) -> 津(三重) -> 津新町(三重) -> 南が丘(三重) -> 久居(三重) -> 桃園(三重) -> 伊勢中川(三重) -> 川合高岡(三重) -> 伊勢石橋(三重) -> 大三(三重) -> 榊原温泉口(三重) -> 東青山(三重) -> 西青山(三重) -> 伊賀上津(三重) -> 青山町(三重) -> 伊賀神戸(三重) -> 美旗(三重) -> 桔梗が丘(三重) -> 名張(三重) -> 赤目口(三重) -> 三本松(奈良) -> 室生口大野(奈良) -> 榛原(奈良) -> 長谷寺(奈良) -> 大和朝倉(奈良) -> 桜井(奈良) -> 大福(奈良) -> 耳成(奈良) -> 大和八木(奈良) -> 真菅(奈良) -> 松塚(奈良) -> 大和高田(奈良) -> 築山(奈良) -> 五位堂(奈良) -> 近鉄下田(奈良) -> 二上(奈良) -> 関屋(奈良) -> 大阪教育大前(大阪) -> 河内国分(大阪) -> 安堂(大阪) -> 堅下(大阪) -> 法善寺(大阪) -> 恩智(大阪) -> 高安(大阪) -> 河内山本(大阪) -> 近鉄八尾(大阪) -> 久宝寺口(大阪) -> 弥刀(大阪) -> 長瀬(大阪) -> 俊徳道(大阪) -> 布施(大阪) -> 今里(大阪) -> 鶴橋(大阪) -> 谷町九丁目(大阪) -> 日本橋(大阪) -> なんば(大阪) -> 桜川(大阪) -> ドーム前(大阪) -> 九条(大阪) -> 西九条(大阪) -> 千鳥橋(大阪) -> 伝法(大阪) -> 福(大阪) -> 出来島(大阪) -> 大物(兵庫) -> 尼崎(兵庫) -> 出屋敷(兵庫) -> 到着
val test1 : unit = ()

kisaragiまたはdeyashikiは存在しません。val test2 : unit = ()

最短距離は24.9です。最短経路は
平田町(三重) -> 三日市(三重) -> 鈴鹿市(三重) -> 柳(三重) -> 伊勢若松(三重) -> 箕田(三重) -> 長太ノ浦(三重) -> 楠(三重) -> 北楠(三重) -> 塩浜(三重) -> 海山道(三重) -> 新正(三重) -> 近鉄四日市(三重) -> 川原町(三重) -> 阿倉川(三重) -> 霞ヶ浦(三重) -> 近鉄富田(三重) -> 到着
val test3 : unit = ()
*)


(* 目的：ローマ字の県名を漢字の県名に直して返す関数 *)
(* kenhenkan : string -> string *)
let kenhenkan b =
  if b = "hokkaido" then "北海道"
  else if b = "aomori" then "青森"
  else if b = "iwate" then "岩手"
  else if b = "miyagi" then "宮城"
  else if b = "akita" then "秋田"
  else if b = "yamagata" then "山形"
  else if b = "fukushima" then "福島"
  else if b = "ibaraki" then "茨城"
  else if b = "tochigi" then "栃木"
  else if b = "gunma" then "群馬"
  else if b = "saitama" then "埼玉"
  else if b = "chiba" then "千葉"
  else if b = "tokyo" then "東京"
  else if b = "kanagawa" then "神奈川"
  else if b = "nigata" then "新潟"
  else if b = "toyama" then "富山"
  else if b = "ishikawa" then "石川"
  else if b = "fukui" then "福井"
  else if b = "yamanashi" then "山梨"
  else if b = "nagano" then "長野"
  else if b = "gifu" then "岐阜"
  else if b = "shizuoka" then "静岡"
  else if b = "aichi" then "愛知"
  else if b = "mie" then "三重"
  else if b = "shiga" then "滋賀"
  else if b = "kyoto" then "京都"
  else if b = "osaka" then "大阪"
  else if b = "hyogo" then "兵庫"
  else if b = "nara" then "奈良"
  else if b = "wakayama" then "和歌山"
  else if b = "tottori" then "鳥取"
  else if b = "shimane" then "島根"
  else if b = "okayama" then "岡山"
  else if b = "hiroshima" then "広島"
  else if b = "yamaguchi" then "山口"
  else if b = "tokushima" then "徳島"
  else if b = "kagawa" then "香川"
  else if b = "ehime" then "愛媛"
  else if b = "kochi" then "高知"
  else if b = "fukuoka" then "福岡"
  else if b = "saga" then "佐賀"
  else if b = "nagasaki" then "長崎"
  else if b = "kumamoto" then "熊本"
  else if b = "oita" then "大分"
  else if b = "miyazaki" then "宮崎"
  else if b = "kagoshima" then "鹿児島"
  else if b = "okinawa" then "沖縄"
    else raise Not_found
    
(*目的：ローマ字の文字列による駅名と県名と駅名リストを受け取ったら、その駅の漢字表記と県名のペアを返す関数*)
(*romaji_to_kanji2 : string -> string -> ekimei_t list -> string * string *)
            
let rec romaji_to_kanji2 a b lst = match lst with
    [] -> ("","")
  |{
    kanji=cal; (* 漢字の駅名 *)
    kana=kan;(* 読み *)
    romaji=rom;(* ローマ字 *)
    ken=ke; (* 県名 *)
    shozoku=sho (* 所属路線名 *)
  }::rest -> if a = rom && (kenhenkan b) = ke then (cal,ke)
    else romaji_to_kanji2 a b rest

(* 起点の（ローマ字の）県名と駅名と終点の（ローマ字の）県名と駅名と駅名リスト（ekimei_t list 型・駅間リスト（ekikan_t list 型を受け取ったら、
romaji_to_kanji2 を使って起点と終点の駅名ペアを求め、
受け取った駅名リストから seiretsu2 を使って重複を取り除き、
make_eki_list2 と shokika2 （または make_initial_eki_list2）を使って 駅のリスト（eki_t list 型）を作り、
dijkstra_main を使って、各駅までの最短路を確定し、
   その中から終点の駅（eki_t 型）を探して返すような関数 *)
(* dijkstra2 : string -> string -> string -> string -> ekimei_t list -> ekikan_t list *)
let dijkstra2 kitken kit shuken shu ekimei_list ekikan_list =
  let sei_ekimei_list = seiretsu2 ekimei_list in
  let (kitkan,kitken),(shukan,shuken) = (romaji_to_kanji2 kit kitken sei_ekimei_list),(romaji_to_kanji2 shu shuken sei_ekimei_list) in
  ekimikke (dijkstra_main (shokika2 (make_eki_list2 sei_ekimei_list) (kitkan,kitken)) (maketree ekikan_list)) (shukan,shuken)

    
(* 最短路が求まったら最短経路と最短距離を「きれいに」出力する関数 *)
(* kirei2 :  string -> string -> string -> string -> ekimei_t list -> ekikan_t list *)
let kirei2 kitken kit shuken shu ekimei_list ekikan_list =
  let {namae = (na,ke); saitan_kyori = kyo; temae_list = lst} = dijkstra2 kitken kit shuken shu ekimei_list ekikan_list in
  if kyo = infinity then
    (print_newline ();
     print_string kit;
     print_string "または";
     print_string shu;
     print_string "は存在しません。";
     print_newline ())
  else
    (print_newline ();
     print_string "最短距離は";
     print_float kyo;
     print_string "kmです。最短経路は";
     keiro lst;
     print_string "到着";
     print_newline ())

