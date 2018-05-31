

#-------------------------------
#       Phương án 2 và 3
#-------------------------------

# TRước hết, nếu chị ưa thích cách làm thủ công thì ráp 
# nối kiểu này là nhanh nhất: https://rpubs.com/chidungkt/351476. 

library(tidyverse)
library(magrittr)
library(raster)

vietnam_province <- getData("GADM", country = "Vietnam", level = 1)
detach(package:raster)
vietnam_df <- vietnam_province %>% fortify(region = "ID_1")

# Mã số các tỉnh. Chú ý rằng số lượng là 63: 

vietnam_df$id %>% n_distinct()
vietnam_df$id %>% unique()

# So với GSO thì cơ quan này dùng 2 chữ số cho mã tỉnh. 
# Vì ta cần làm việc với đữ liệu của GSO nên phải chuẩn 
# hóa một trong hai thằng (nguồn: http://www.gso.gov.vn/dmhc2015/). 
# Từ đó ta viết hàm thêm số 0 vào trước, ví dụ, số 1: 

library(stringr)

add_more_zero <- function(x) {
  ELSE <- TRUE
  case_when(str_count(x) == 1 ~ paste0("0", x), 
            ELSE ~ x)
}

# Sử dụng hàm vừa viết tạo một cột biến tên là id_new: 
vietnam_df %<>% mutate(id_new = add_more_zero(id))


# Thằng id_new này đóng vai trò trung gian để, ví dụ, ráp tên 
# các tỉnh theo quyu định của GSO. Nhưng khi chị vẽ bản đồ thì
# chị NÊN  sử dụng id chứ không phải id_new. Trước hết lấy
# dữ liệu từ GSO (nếu không quen chị có thể down Excel File về): 


library(rvest)

gso_link <- "http://www.gso.gov.vn/dmhc2015/"


# Mã các tỉnh theo GSO: 
id_new <- gso_link %>% 
  read_html() %>% 
  html_nodes(".dxgvDetailButton_Office2003_Blue+ .dxgv") %>% 
  html_text() 

# Tên các tỉnh theo GSO: 

prov_names <- gso_link %>% 
  read_html() %>% 
  html_nodes("#ctl00_PlaceHolderMain_grid1_DXMainTable .dxgv:nth-child(3)") %>% 
  html_text()

# Xem qua: 
prov_names %>% head()

# Tạo DF mới với data từ GSO và Bịa thêm, ví dụ, dữ liệu về đói nghèo: 

df_prov_names <- data.frame(id_new = id_new, 
                            prov_names = prov_names, 
                            poverty = runif(length(prov_names), 0, 30) %>% round(0))

# Cẩn thận chỉ chuyển cột biến nếu ở factor về character: 
df_prov_names %<>% mutate_if(is.factor, as.character)


# Giờ có thể ráp chúng lại: 

vietnam_df_name_code <- right_join(vietnam_df, df_prov_names, by = "id_new")

# Rất có thể chị cần hiển thị các tỉnh theo kiểu Latin không dấu. 
# Nếu thế tạo thêm cột biến mới đồng thời bỏ chữ "tỉnh" đi: 

library(stringi)

vietnam_df_name_code %<>% mutate(prov_name_Latin = stri_trans_general(prov_names, "Latin-ASCII"))

# Hậu quả là: 

vietnam_df_name_code$prov_name_Latin %>% unique()

# Viết cái hàm xóa "Tinh" và "Thanh pho". Cũng đề phòng trường
# hợp có hai dấu cách liên tiếp thì thay bằng một dấu cách thôi: 

my_replace <- function(x) {
  x %>% 
    str_replace_all("Thanh pho ", "") %>% 
    str_replace_all("Tinh ", "") %>% 
    str_replace_all("city", "") %>% 
    str_replace_all("-", "") %>% 
    str_replace_all("  ", " ") %>% 
    str_trim()
}

# Sử dụng hàm và kiểm tra: 

vietnam_df_name_code %<>% mutate(prov_name_Latin = my_replace(prov_name_Latin))
vietnam_df_name_code$prov_name_Latin %>% unique()

# Vẽ cái đồ hành chính đơn thuần: 

ggplot() +
  geom_polygon(data = vietnam_df_name_code,
               aes(long, lat, group = group, fill = id),
               show.legend = FALSE) +
  coord_equal()

# Rõ ràng chúng ta có cái gì sai ở đây. NGuyên nhân là những con số lấy 
# từ dữ liệu bản đồ nó chỉ thuần túy là cách kí hiệu (từ 1 đến 63) chứ không
# có ý nghĩa hành chính kiểu như GSO. Điều này gợi ý rằng chúng ta nên 
# ráp chúng lại theo id kiểu tỉnh chứ không nên là id kiểu số. Nếu thế: 

vietnam_df_cha <- vietnam_province %>% fortify(region = "NAME_1")

vietnam_df_cha$id %>% unique()


# Vậy thì nên tạo một cột biến mới: 

df_prov_names %<>% mutate(prov_names_Latin = prov_names %>% 
                            stri_trans_general("Latin-ASCII") %>% 
                            my_replace())

# Kết quả: 
df_prov_names %>% head()

# Tương tự: 

vietnam_df_cha %<>% mutate(prov_names_Latin = id %>% 
                             stri_trans_general("Latin-ASCII") %>% 
                             my_replace())

# So sánh thì thấy: 

df_prov_names$prov_names_Latin %>% unique() ->> gso_names # Của GSO. 
vietnam_df_cha$prov_names_Latin %>% unique() ->> map_names # Của bản đồ. 

gso_names
map_names

# Để chắc chắc hơn cần đảm bảo rằng không có chữ FLASE nào ở đây: 
gso_names[order(gso_names)] == map_names[order(map_names)]


# Vậy là OK rồi. Có thể tự tin mà ráp nối: 


my_df <- right_join(vietnam_df_cha, df_prov_names, by = "prov_names_Latin")


# Bản đồ hành chính: 
ggplot() +
  geom_polygon(data = my_df,
               aes(long, lat, group = group, fill = id),
               show.legend = FALSE) +
  coord_equal()

# Bản đồ đói nghèo: 

library(viridis)
ggplot() +
  geom_polygon(data = my_df,
               aes(long, lat, group = group, fill = poverty),
               color = "white", size = 0.01) +
  coord_equal() + 
  scale_fill_viridis(direction = -1, option = "D", "Poverty")


# Kết luận: Cái chốt của vấn đề nối dữ liệu là, làm sao phải có cái 
# gì chung giữa hai data frame. Cách thức gõ thủ công bằng tay sẽ 
# không khả thi nếu chị làm cho cấp xã. Cách thức tốt hơn là chuẩn 
# hóa, ví dụ, tên tỉnh (nếu làm ở cấp tỉnh) cho giống nhau ở hai 
# data frame. 



