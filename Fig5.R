##### Fig5 #####
load("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/Fig5ac.RData")
load("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/Fig5bd.RData")

# reduce top and bottom margins
empty <- ggplot() + theme_void()
dbar <- dbar + theme(plot.margin = unit(c(-35, 10, -30, 10), "pt"))
dbar_smaller <- grid.arrange(empty, dbar, empty , ncol=3, widths = c(1,4,1))

a_cluster <- ggplotGrob(a_cluster)
a_bar_mean_dom_var_cluster <- ggplotGrob(a_bar_mean_dom_var_cluster)
gt1_a <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_a <- gtable_add_grob(gt1_a, a_cluster, t=2, b=32, l=1, r=32)
gt1_a <- gtable_add_grob(gt1_a, a_bar_mean_dom_var_cluster, t = 29, l = 3, b = 19, r = 10)
# grid.draw(gt1_a)

b_cluster <- ggplotGrob(b_cluster)
b_bar_mean_dom_var_cluster <- ggplotGrob(b_bar_mean_dom_var_cluster)
gt1_b <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_b <- gtable_add_grob(gt1_b, b_cluster, t=2, b=32, l=1, r=32)
gt1_b <- gtable_add_grob(gt1_b, b_bar_mean_dom_var_cluster, t = 29, l = 3, b = 19, r = 10)
# grid.draw(gt1_b)

cc <- ggplotGrob(cc)
c_bar_mean_rank <- ggplotGrob(c_bar_mean_rank)
gt1_c <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_c <- gtable_add_grob(gt1_c, cc, t=4, b=32, l=1, r=32)
gt1_c <- gtable_add_grob(gt1_c, c_bar_mean_rank, t = 29, l = 3, b = 19, r = 10)
# grid.draw(gt1_c)

dd <- ggplotGrob(dd)
d_bar_mean_rank <- ggplotGrob(d_bar_mean_rank)
gt1_d <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_d <- gtable_add_grob(gt1_d, dd, t=4, b=32, l=1, r=32)
gt1_d <- gtable_add_grob(gt1_d, d_bar_mean_rank, t = 29, l = 3, b = 19, r = 10)
# grid.draw(gt1_d)


plot_ab <- grid.arrange(plot_grid(gt1_a,gt1_b,labels = c("a)","b)"), ncol = 2, label_size = 20),
                        grid.arrange(empty,a_legend,empty, ncol = 3, widths = c(.15,.7,.15)),
                        nrow = 2, heights = c(1,.4))
plot_cd <- grid.arrange(plot_grid(gt1_c,gt1_d,labels = c("c)","d)"), ncol = 2, label_size = 20),
                        grid.arrange(empty, dbar_smaller, empty, ncol=3, widths = c(.15,.7,.15)),
                        nrow = 2, heights = c(1,.4))

plot <- grid.arrange(plot_ab, plot_cd,
                     nrow = 2)



ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/Fig5.png"), plot = plot, width = 18, height = 14, units = "in")
