##### Fig5 #####
load("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/Fig4ac.RData")

# This one you still have to generate. You don't need the legends again, but the rest you should be able to copy
# load("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/Fig4bd.RData")

# reduce top and bottom margins
empty <- ggplot() + theme_void()

a_cluster <- ggplotGrob(a_cluster)
a_bar_expl_extr <- ggplotGrob(a_bar_expl_extr)
gt1_a <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_a <- gtable_add_grob(gt1_a, a_cluster, t=1, b=32, l=1, r=32)
gt1_a <- gtable_add_grob(gt1_a, a_bar_expl_extr, t = 29, l = 3, b = 19, r = 10)
# grid.draw(gt1_a)

# for illustration purposes, I just replace b) with a)
b_cluster <- a_cluster
b_bar_expl_extr <- a_bar_expl_extr
# b_cluster <- ggplotGrob(b_cluster)
# b_bar_expl_extr <- ggplotGrob(b_bar_expl_extr)
gt1_b <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_b <- gtable_add_grob(gt1_b, b_cluster, t=1, b=32, l=1, r=32)
gt1_b <- gtable_add_grob(gt1_b, b_bar_expl_extr, t = 29, l = 3, b = 19, r = 10)
# grid.draw(gt1_b)

cc <- ggplotGrob(cc)
c_bar_rank <- ggplotGrob(c_bar_rank)
gt1_c <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_c <- gtable_add_grob(gt1_c, cc, t=4, b=32, l=1, r=32)
gt1_c <- gtable_add_grob(gt1_c, c_bar_rank, t = 29, l = 3, b = 19, r = 10)
# grid.draw(gt1_c)

# for illustration purposes, I just replace b) with a)
dd <- cc
d_bar_rank <- c_bar_rank
# dd <- ggplotGrob(dd)
# d_bar_rank <- ggplotGrob(d_bar_rank)
gt1_d <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_d <- gtable_add_grob(gt1_d, dd, t=4, b=32, l=1, r=32)
gt1_d <- gtable_add_grob(gt1_d, d_bar_rank, t = 29, l = 3, b = 19, r = 10)
# grid.draw(gt1_d)


plot_ab <- grid.arrange(plot_grid(gt1_a,gt1_b,labels = c("a)","b)"), ncol = 2, label_size = 20),
                        grid.arrange(empty,a_legend,empty, ncol = 3, widths = c(.15,.7,.15)),
                        nrow = 2, heights = c(1,.4))
plot_cd <- grid.arrange(plot_grid(gt1_c,gt1_d,labels = c("c)","d)"), ncol = 2, label_size = 20),
                        grid.arrange(empty,c_legend,empty, ncol = 3, widths = c(.15,.7,.15)),
                        nrow = 2, heights = c(1,.4))

plot <- grid.arrange(plot_ab, plot_cd,
                     nrow = 2)



ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/Fig4.png"), plot = plot, width = 18, height = 14, units = "in")
