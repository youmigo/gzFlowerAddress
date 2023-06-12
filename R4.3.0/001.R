# Thu Jun  8 18:51:30 2023 edit
# 字符编码：UTF-8
# R 版本：R 4.1.1 x64 for window 11
# cgh163email@163.com
# 个人笔记不负责任，拎了个梨🍐🍈
rm(list = ls());gc()
require(tidyr)
require(DT)
require(leaflet)
require(leafletCN)
library(crosstalk)
require(leafpop)
library(DT)
require(leafem)


# dt <- read.csv('https://gitee.com/youmigo/gz_flowers/raw/master/data/%E5%B9%BF%E5%B7%9E%E8%B5%8F%E8%8A%B1%E5%85%A8%E6%94%BB%E7%95%A5.csv',header = T,sep = ',',encoding = 'UTF-8')
# tidyr::unite( dt,"inf",name,Month,add,sep = "\n🍐")[,2] |> #合并列
#   head()
dt <- readr::read_csv('data/gzflower.csv') |>
  separate(#data = dt,
         col = xy,remove=F,
         sep = ',',
         into = c('lng', 'lat')) |>  #分割列分列
unite('infs',names,M,add,inf,sep = "\n🍐")

# names(dt) <- c('id','name','Month','city','add','lng','lat')


dt[seq(1, dim(dt)[1], by = 2) ,] |> as.data.frame() |> DT::datatable(rownames = F)

#Thu Jun  8 19:35:12 2023 --
gz.map <-  'https://geo.datav.aliyun.com/areas_v3/bound/geojson?code=440100_full' |>
  sf::st_read()
#出图地图  Thu Jun  8 19:45:46 2023 ------------------------------

mle <-
leaflet() |>
  amap() |>
  addPolylines(data =
                gz.map,
               color = 'green'
  ) |>
  addCircleMarkers(
    data = dt,
    lat = ~ lat |> as.numeric(),
    lng = ~ lng |> as.numeric(),
    # popup = ~ infs
    popup = popupTable(dt[,2:3]) #  超级弹窗
  ) |>
  addLogo( offset.x = 50,
           offset.y = 50,"http://image.izyz.org/obs-izy//upload/orgRegisterPic/idcard/20220829/1661777579820.jpg",
          url = "https://youmigo.gitee.io/gz_flowers/")
mle
#ok Thu Jun  8 19:35:25 2023 --

#出表 Thu Jun  8 19:39:51 2023 ------------------------------

dt2 <-dt[,2:3]
dtsd <- SharedData$new(dt2)
ft <- datatable(dtsd$data(),
          ,  colnames  = c('区', '花名+月份+地点+详情')
          # ,  caption = '广州赏花全攻略地图联动表2.1'
          ,  caption =
            htmltools::tags$caption(
              # htmltools::h2("广州赏花全攻略地图联动表2.1 "))
              htmltools::h2("广州赏花全攻略地图联动表3.1 "))
          , escape = FALSE
          ,filter = 'top'
          )
ft
htmlwidgets::saveWidget(ft, file='web/ft.html')
# Thu Jun  8 19:45:55 2023 --
#合并 Thu Jun  8 19:46:01 2023 ------------------------------
bscols(
  widths = c(5,7),
  ft,mle
  , device = c( "lg"))#"xs", "sm", "md",


#高德导航 Thu Jun  8 19:48:58 2023 ------------------------------
xy <- dt$xy
label <- dt$infs
amapurl <-
  data.frame( "<a href='",
              "https://uri.amap.com/marker?position=",
              xy,
              '&name=',
              label,
              '&src=🍐&coordinate=&callnative=1',
              '[\']target=\"_blank\">',
              label,
              "</a>"  )
names(amapurl) <- letters[1:length(amapurl)]
dt2 <-
  tidyr::unite(amapurl,"laburl",names(amapurl),sep = "",remove = T) |> cbind(dt2)
rm(xy,amapurl,label)
#pop标签 Tue Jul 26 01:52:22 2022 ------------------------------
myicon <- makeIcon(iconUrl = "data/icon.svg",
                   iconWidth = 50.45, iconHeight = 50.20)
# Thu Jun  8 19:57:14 2023 --
dt2$xy <- dt$xy
dt2$lat <- dt$lat |> as.numeric()
dt2$lng <- dt$lng |> as.numeric()
dtsd <- SharedData$new(dt2)

#准备出图 Thu Jun  8 19:54:58 2023 ------------------------------
mpurl <-
  leaflet(data = dtsd) |>
  amap() |>
  addPolygons(data = gz.map, # 广州地图
              fillColor = rainbow(11)
              ,color = 'green') |>
  addCircleMarkers( #底圈，用于分类。
    # data = dtsd,
    lat = ~ lat,
    lng = ~ lng,
    popup = ~ infs,
    color = 'green'
    # ,      colorRampPalette(
    #       c('#99CCFF', '#996600'))(dt |>nrow()) #  按行数生成颜色数
    , clusterOptions = markerClusterOptions() #  放遮盖
  ) |>
  # addCircleMarkers(113.433164, 23.627496, popup = "<a href='/html/gzatable.html' target=\"_blank\">查看景点列表</a>") |>
  addLogo("https://z3.ax1x.com/2021/10/09/5iF8Vx.png",url = "ft.html") |>
  addMarkers( #标注点
    # data = dtsd,
    lat = ~ lat,
    lng = ~ lng,
    popup = ~ laburl
    ,icon = myicon #  pop图表ico
  )
mpurl

htmlwidgets::saveWidget(mpurl, file='web/index.html')
#合并 Thu Jun  8 20:13:08 2023 ------------------------------
ft2 <- datatable(dtsd$data()[,2:3])
bscols(
  widths = c(5,7),
  ft,mpurl
  , device = c( "lg"))#"xs", "sm", "md",
