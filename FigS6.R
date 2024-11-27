##### FigS6 #####
load("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/FigS6.RData")
load("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/FigS6b.RData")

# reduce top and bottom margins
empty <- ggplot() + theme_void()

aa <- ggplotGrob(aa)
a_bar_mean_dom_var <- ggplotGrob(a_bar_mean_dom_var)
gt1_a <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_a <- gtable_add_grob(gt1_a, aa, t=1, b=32, l=1, r=32)
gt1_a <- gtable_add_grob(gt1_a, a_bar_mean_dom_var, t = 29, l = 3, b = 19, r = 10)
# grid.draw(gt1_a)

bb <- ggplotGrob(bb)
b_bar_mean_dom_var <- ggplotGrob(b_bar_mean_dom_var)
gt1_b <- gtable(widths = unit(rep(2,32), c("null")), heights = unit(rep(2,32), "null"))
gt1_b <- gtable_add_grob(gt1_b, bb, t=1, b=32, l=1, r=32)
gt1_b <- gtable_add_grob(gt1_b, b_bar_mean_dom_var, t = 29, l = 3, b = 19, r = 10)
# grid.draw(gt1_b)


plot_ab <- grid.arrange(plot_grid(gt1_a,gt1_b,labels = c("a)","b)"), ncol = 2, label_size = 20),
                        grid.arrange(empty,a_legend,empty, ncol = 3, widths = c(.15,.7,.15)),
                        nrow = 2, heights = c(1,.4))



ggsave(paste0("/Net/Groups/BGI/people/jdenis/scripts/scripts_Nature_Perspective/FigS6.png"), plot = plot_ab, width = 18, height = 7, units = "in")
