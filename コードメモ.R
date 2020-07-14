
library(tidyverse)
library(magrittr)

# クロス表成型用の関数 --------------------------------------------------------------
# 調査実習に準拠した情報を盛り込んだクロス表を出力します
# listではなくdata.frameで出力することで，excelへの出力を簡単にします

my_cross <- function(x){
  crosstab <- 
    x %>% 
    janitor::adorn_totals(where = c('row', 'col')) %>%
    janitor::adorn_percentages(denominator = 'row') %>% 
    dplyr::as_tibble() %>% 
    dplyr::mutate_at(.vars = vars(-1), .funs = ~{round(.*100, 1)})
  
  N <- 
    x %>% 
    janitor::adorn_totals(where = c('row', 'col')) %>%
    dplyr::as_tibble() %>% 
    dplyr::select(N = Total)
  
  cramer <-
    x %>% 
    janitor::untabyl() %>%
    dplyr::select(-1) %>%
    as.matrix() %>%
    vcd::assocstats() %>% 
    .$cramer
  
  p.value <-
    x %>% 
    janitor::chisq.test() %>% 
    .$p.value
  
  dplyr::bind_cols(crosstab, N) %>% 
    dplyr::mutate(cramer = c(cramer, rep(NA_real_, nrow(.) - 1)),
                  p.value = c(p.value, rep(NA_real_, nrow(.) - 1)))
}

# 使用例
# daiamondsのcutとclarityのクロス表を作成
diamonds %>% 
  janitor::tabyl(cut, clarity) %>% 
  # my_crossを適用
  my_cross() %>% print
  # csvで出力
  write_excel_csv(path = 'output/diamonds_cross.csv')

  

# クロス表の調整残差の算出 ------------------------------------------------------------

# chisq.testのstdresがそれ 
diamonds %>% 
  tabyl(cut, clarity) %>% 
  chisq.test() %>% .$stdres
  

# カテゴリカル変数の記述統計 -----------------------------------------------------------
# 連続変数はpsych::describeとかでいいけどカテゴリカル変数は良いのがなかったので

describe_d <-
    function(data){
      purrr::map2_dfr(.x = data, .y = names(data),
               .f =  ~{
                 tab <- janitor::tabyl(.x)
                 variable_name <- .y
                 N <- tab[,2] %>% sum()
                 dplyr::tibble(
                   variables = c(variable_name,
                                 stringr::str_c('  ', tab[,1])),
                   N = c(N, tab[,2]),
                   percent = c(1, tab[,3]))
                 })
      }
  
# 使用例
diamonds %>% 
  select(cut, clarity) %>% # 記述統計を算出したい変数を選択
  describe_d()

  
# 日本語と正規表現 ----------------------------------------------------------------
# stringrで日本語の任意の文字がマッチしない問題
# （日本語は使うなと言われる所以かもしれないが，スクレイピングとかではどっちみち困ってしまう）
# （例）
x <- c('hello world')
str_detect(x, pattern = 'hello')
y <- '新型コロナウイルス'
str_detect(y, '新型コロナウイルス') #なぜかFALSEになる

# 現状では日本語の時はbaseR(gsubとかgrepl)を使うのが無難っぽい
grepl('新型コロナウイルス', y)

# ただpipeでやや使いにくいのと，
# やはり羽鳥教の信者としてはstringrで処理したいという欲はある


# 全角英数を半角英数に --------------------------------------------------------------
x <- c('３', '4４', '45５', '（') %>% iconv(to = 'UTF-8')
stringi::stri_trans_nfkc(x)



# pipeのちょい技 ---------------------------------------------------------------

# グラフとデータを同時に出力したいとき
iris %>% 
  as_tibble() %>% 
  {
    print(.)
    ggplot(data = .)+
      geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = Species))
  }
# {}を使って，処理を分岐できる
# PDF scraping -------------------------------------------------
browseURL('https://blog.az.sg/posts/reading-pdfs-in-r/') # 参考
# entry1: tesseract
# pdfにocrをかけるパッケージ
# 日本語データのダウンロード
# tesseract::tesseract_download('jpn')

text <- tesseract::ocr('https://www.mhlw.go.jp/content/10906000/000610593.pdf', engine = 'jpn')
text %>% 
  str_split('\n')

# あんまりうまくいってない
# そもそもテキストがすでに入ってるpdfを一度pngに変換してからocrをかけるので，
# 用途が違う気がする

# entry2: pdftools
# pdf_dataがそれっぽいが…
pdftools::pdf_data('https://www.mhlw.go.jp/content/10906000/000610593.pdf') %>% 
  flatten_dfr() %>% 
  view()
# どうやらテキストボックス単位で位置情報とかを取り出すだけで使いにくそう

# 素直にpdf_text
text <- pdftools::pdf_text('https://www.mhlw.go.jp/content/10906000/000610593.pdf')
  
text %>% 
  # 改行部分で区切る
  str_split('\n') %>% 
  as_tibble(.name_repair = 'unique')
# 後は正規表現とかで頑張るしかないか…




# print_all ---------------------------------------------------------------

print_all <- 
  function(x){
    print(x, n = Inf)
  }

# 型変換_type_convert関数 ---------------------------------------------------------------------
# 便利なんだけど使えなさそう
x <- tibble(A = c("1", "2", "3", "4"),
            B = c("2020-03-01", "2020-03-02", "2020-03-03", "2020-03-04"),
            C = c("あ", "い", "う", "え"))

# 今まで→ひとつづつ型指定
x %>% 
  mutate(A = parse_number(A),
         B = parse_date(B)) 
# 便利な関数readr::type_convert
x %>% 
  type_convert()

# 自分で打った文字は基本的にCP932で認識される
# UTF-8のデータセットに，追加で日本語を入れるとUTF-8とCP932が混在してヤバイ？

# e-statAPI ---------------------------------------------------------

library(estatapi)
# 参考
# browseURL('https://qiita.com/kazutan/items/9c0b2dd0f055fde45cda')

#アプリIDの入力
appid <- "0ae27e0eb2f0eca4f893189bc721ff644eaca847"

#キーワードを含む統計情報を表示：estat_getStatsListコマンド
estat_getStatsList(appId = appid, 
                             searchWord = "進学率")

# 統計情報のリストから欲しいデータセットのidを次に記入
meta <- estat_getMetaInfo(appId = appid, statsDataId = '0003147040')

# 実際のデータを取り出す
# 欲しい情報を「'cd' + meta_infoのリスト名 = ID」の形で指定
data <- estat_getStatsData(appId = appid, 
                   statsDataId = '0003147040', 
                   cdCat02 = c('0000000060', '0000000070'))

# 分析例：大学進学率の推移
data %>% 
  transmute(year = parse_number(`時間軸（年次）`),
            性別, 
            進学率 = value, 
            区分 = `学校種別（年次統計　進学率）`) %>% {
              filter(., 性別 == '計') %>% print(n = 20)
              ggplot(., aes(year, 進学率, color = 性別))+
                geom_point()+
                geom_line()+
                scale_x_continuous(breaks = seq(1950, 2020, 10))+
                facet_wrap(~区分)
}

# 日付・時刻の変換 -------------------------------------------------------------------
# 参考：'https://uribo.hatenablog.com/entry/2017/07/13/085000'
x <- tibble(date = c('2020年4月5日', '2020年5月21日') %>% str_conv('CP932'))

# 今まで
x %>% 
  separate(col = date, into = c('year', 'month', 'day'), sep = '\\p{Han}') %>% 
  mutate_at(vars(c(month, day)), ~{str_pad(., width = 2, pad = '0')}) %>% 
  unite(col = 'date', c(year, month, day), sep = '-') %>% 
  mutate(date = parse_date(date))

# 便利な関数の使い方:parse_dateのformat
x %>% 
  mutate(date = parse_date(date, format = str_conv('%Y年%m月%d日', 'CP932')))

# 例2
parse_date(str_conv('2020/04/05', 'CP932'), format = str_conv('%Y/%m/%d', 'CP932'))

# parse_datetimeも同様
parse_datetime(str_conv('2015年3月4日　15時32分', 'CP932'),
               format = str_conv('%Y年%m月%d日　%H時%M分', 'CP932'),
               locale = locale(tz = 'Asia/Tokyo'))


# 潜在クラス分析 -----------------------------------------------------------------
# 参考：https://qiita.com/saltcooky/items/dc48ca3cefa9c1dfc010

library(poLCA)
# poLCAに入っているcarcinomaデータを使う
# 子宮頚部に癌腫が存在するか否かについて、118患者に対する7人の病理学者による二分法の評価データらしい

data("carcinoma")

df <- 
  carcinoma %>% 
  as_tibble() 

# データ確認
df %>% 
  glimpse()

# 簡単な可視化
df %>% 
  pivot_longer(cols = A:G) %>% 
  ggplot(aes(value))+
  geom_bar()+
  facet_wrap(~name)
  
# 潜在クラス分析
# formulaはcbind()~1で書く（なんだこりゃ）
formula <- 
  df %$%
  cbind(A, B, C, D, E, F, G)~1

# モデル選択（broom対応しててうれしい）
# ALERT: iterations finished, MAXIMUM LIKELIHOOD NOT FOUND 
# というアラートに注意する。これが出たときは最尤推定が上手くいっていないので，
# maxiterの値を多くして回してみる
result <- 
  enframe(1:4, name = 'model.no', value = 'class') %>% 
  mutate(model = map(class, ~{poLCA(formula = formula, data = df, nclass = ., maxiter = 3000)}))
         
result %>% 
  mutate(glanced = map(model, broom::glance)) %>% 
  unnest(glanced)
# BICやg.squaredからモデルを選択する
# 今回は3クラスがいいっぽい

# 3クラスの結果を見る
result %>% 
  filter(class == 3) %>% 
  transmute(tidied = map(model, broom::tidy)) %>%
  unnest(tidied) %>% 
  mutate_if(.predicate = is.numeric, .funs = ~{round(., digits = 3)}) %>% 
  select(-std.error) %>% 
  pivot_wider(names_from = class, values_from = estimate)
  print(n = Inf)



  
# 都道府県正規表現 ----------------------------------------------------------------

'(北海道|青森県|岩手県|宮城県|秋田県|山形県|福島県|茨城県|栃木県|群馬県|埼玉県|千葉県|東京都|神奈川県|新潟県|富山県|石川県|福井県|山梨県|長野県|岐阜県|静岡県|愛知県|三重県|滋賀県|京都府|大阪府|兵庫県|奈良県|和歌山県|鳥取県|島根県|岡山県|広島県|山口県|徳島県|香川県|愛媛県|高知県|福岡県|佐賀県|長崎県|熊本県|大分県|宮崎県|鹿児島県|沖縄県)'



  


# RMeCab ------------------------------------------------------------------

library(RMeCab)

a <- RMeCabC("すもももももももものうち")

# データフレームにしておく
flatten_chr(a) %>% 
  enframe()
             

# rtweet ------------------------------------------------------------------

library(rtweet)
library()
x <- get_timeline(user = 'Peacefulbassist', n = 2000)


# RMeCabで形態素解析をしてみる
x %>% 
  select(text) %>% 
  RMeCabC() %>% 
  flatten_chr() %>% 
  enframe() %>% 
  filter(name == '名詞') %>% 
  filter(str_detect(value, '(\\p{Han}|\\p{Hiragana}|\\p{Katakana})')) %>% 
  count(value, sort = T) %>% print(n = 100)
  

