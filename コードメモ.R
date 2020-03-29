library(tidyverse)


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


