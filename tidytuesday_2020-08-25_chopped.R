library(tidyverse)
library(igraph)
library(BBmisc)
library(tm)
library(colorspace)
library(patchwork)
library(extrafont)

chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

#yaaaaa network analysis woohoo
#get the ingrediences

tmp_ingredients <- chopped %>%
  select(appetizer, entree, dessert)

ingredients <- data.frame(ingred = c(tmp_ingredients$appetizer, 
                                     tmp_ingredients$entree, 
                                     tmp_ingredients$dessert)) %>%
    drop_na() %>%
    transmute(ingred_list = str_split(ingred, ","))
  
#build relational table
ingredient_table <- data.frame(Var1 = character(), Var2 = character())
for (x in 1:nrow(ingredients)){
  tmp_combos <- data.frame(t(data.frame(combn(ingredients$ingred_list[[x]], 2))))
  names(tmp_combos) <- c("Var1", "Var2")
  ingredient_table <- bind_rows(ingredient_table, tmp_combos)
}

#trim whitespace, sort, table-ize
ingredient_table_final <- ingredient_table %>%
  mutate(Var1 = trimws(removePunctuation(as.character(Var1)), which = "both")) %>%
  mutate(Var2 = trimws(removePunctuation(as.character(Var2)), which = "both")) %>%
  mutate(sort_list = lapply(1:nrow(.), function(x) sort(c(Var1[x], Var2[x])))) %>%
  mutate(Var1 = as.character(lapply(.$sort_list, function(x) x[1]))) %>%
  mutate(Var2 = as.character(lapply(.$sort_list, function(x) x[2]))) %>%
  select(-sort_list)

all_combos <- data.frame(table(ingredient_table_final$Var1, ingredient_table_final$Var2)) %>%
  filter(Freq > 0)
names(all_combos) <- c("from", "to", "weight")

#swag, now make a network
all_combos$from <- as.character(all_combos$from)
all_combos$to <- as.character(all_combos$to)

ingredient_graph <- graph_from_data_frame(d = all_combos, directed = FALSE)

#there are 107 connected components, 
#but for plotting we're only interested in the biggest with 3865 ingredients
print(components(ingredient_graph))

#pull out largest component
split_subgraphs <- decompose.graph(ingredient_graph)
largest_subgraph <- split_subgraphs[[1]]

#calculate degree and betweenness
deg = degree(largest_subgraph)
bt = normalize(betweenness(largest_subgraph, directed = FALSE), method = "range", range = c(1, 15))
edge_bt = edge_betweenness(largest_subgraph, directed = FALSE)

#change layout
coords <- layout_(largest_subgraph, 
                  with_lgl(root = which(bt == max(bt)),
                           coolexp = 5))

#choose colors
pal <- choose_palette()
colors <- rev(pal(length(unique(deg))))
translational_colors <- tibble(deg = sort(unique(deg)), color = colors)
deg_colors_df <- data.frame(deg = deg) %>%
  left_join(translational_colors)

edge_pal <- choose_palette()
edge_colors <- rev(edge_pal(length(unique(edge_bt))))
edge_tran_colors <- tibble(bt = sort(unique(edge_bt)), color = edge_colors)
bt_colors_df <- data.frame(bt = edge_bt) %>%
  left_join(edge_tran_colors)

#now, make 'legends' for betweenness and degree
top_bt <- bt[order(bt, decreasing=TRUE)[1:5]]
top_deg <- deg[order(deg, decreasing=TRUE)[1:5]]
xy_coords <- data.frame(x = 1, y = 1:5)

ordered_deg_colors_df <- deg_colors_df %>%
  arrange(desc(deg))

#betweenness
p2 <- ggplot(xy_coords) + geom_point(aes(x = x, y = y),
                               size = rev(top_bt),
                               alpha = .8) +
  geom_text(aes(x = x, y = y + .5),
            label = names(rev(top_bt)),
            family = "Berlin Sans FB", size = 8) +
  theme_void() +
  theme(plot.title = element_text(hjust = .5, family = "Cooper Black", 
                                  size = 18, color = colors[30])) +
  labs(title = "Most\ncentral\ningredients") 

#degree
p3 <-ggplot(xy_coords) + geom_point(aes(x = x, y = y), size = 15,
                               color = ordered_deg_colors_df$color[1:5]) +
  geom_text(aes(x = x, y = y + .5),
            label = names(rev(top_deg)),
            family = "Berlin Sans FB", size = 8) +
  theme_void() +
  theme(plot.title = element_text(hjust = .5, family = "Cooper Black", 
                                  size = 18, color = colors[30])) +
  labs(title = "Most\nconnected\ningredients")

#plot network
p2 + 
wrap_elements(panel = ~plot.igraph(largest_subgraph, vertex.label = NA,
                                    vertex.color = deg_colors_df$color,
                                    vertex.size = bt,
                                    vertex.frame.color = NA,
                                    edge.color = bt_colors_df$color,
                                    layout = coords,
                                    margin = -.1), clip = TRUE) +
p3 + 
plot_layout(widths = c(2, 10, 2)) +
  plot_annotation(title = "What are the most iconic ingredients on Chopped?",
                  subtitle = "Over its 45 seasons, the show has used 4,183 different ingredients.
This network includes 3,704, drawing lines between those that appear together in a challenge",
                  caption = "Data from Kaggle courtesy of Jeffrey Braun, visualization @conkshelll",
                  theme = theme(plot.title = element_text(family = "Cooper Black", size = 32,
                                                          color = colors[14]),
                                plot.subtitle = element_text(size = 22, family = "Berlin Sans FB",
                                                             margin = margin(5, 0, 20, 0)),
                                plot.caption = element_text(size = 12, family = "Berlin Sans FB"),
                                plot.background = element_rect(fill = "#FFFFFF")))


