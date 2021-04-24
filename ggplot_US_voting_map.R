library(tidyverse)
library(maps)
library(mapproj)
world_data<-map_data("world")%>%
  as_tibble()
world_data
#'tibble() is a nice way to create data frames.
#' It encapsulates best practices for data frames:
#' It never changes an input's type tibble() will automatically recycle inputs of length 1, 
#' and allows you to refer to variables that you just created
#' foreg: tibble
 tibble(
x = 1:5, 
y = 1, 
z = x ^ 2 + y
)
#world base
 world_base<-world_data %>%
   ggplot()+
   geom_map(
     aes(long,lat,map_id=region),
     map=world_data,
     color="gray80",
     fill="gray30",
     size=0.3
   )
 world_base
world_base+
  coord_map("ortho",orientation = c(39,-98,0))
US_tbl<-map_data("state")%>% as_tibble()
US_tbl
US_tbl%>% ggplot(aes(long,lat,map_id=region))+
  geom_map(map=US_tbl,
           color="gray80",fill="gray30",size=0.3)+
coord_map("ortho",orientation = c(39,-98,0))
#'now adding data to the map 
#'step1 using dplyr package to wrangel the data
#'step2 visualise the data using ggplot2
#'
repub_votong_tbl<- maps::votes.repub%>%
  as_tibble(rownames="state")%>%
  select(state,`1976`)%>%
  rename(repub_prop=`1976`)%>%
  mutate(repub_prop=repub_prop/100)%>%
  mutate(state=str_to_lower(state))
usa_voting_tbl<-US_tbl%>% left_join(repub_votong_tbl,by=c("region"="state"))  
usa_voting_tbl
usa_voting_tbl%>%ggplot(aes(long,lat,group=subregion))+
  geom_map(aes(map_id=region),
           map=US_tbl,
           color="gray80",
           fill="gray30",
           size=0.3)+
  coord_map("ortho",orientation = c(39,-98,0))+
  geom_polygon(aes(group=group,fill=repub_prop),color="black")+
  scale_fill_gradient2(low="blue",mid="white",high="red",
                       midpoint=0.5,labels=scales::percent)+
  theme_minimal()+
  labs(title="voting republic in 1976",
       x="",y="",fill="")+
  theme(plot.title=element_text(size=26,face = "bold",color="red3"),
        legend.position = "bottom"
        )

