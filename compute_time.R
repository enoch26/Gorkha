all_mod_nm <- list(
  fit1a = "fit1a" ,
  fit2a = "fit2a" ,
  fit3a = "fit3a" ,
  fit4a = "fit4a" ,
  fit5a = "fit5a" ,
  fit6a = "fit6a" ,
  fit1b = "fit1b" ,
  fit2b = "fit2b" ,
  fit3b = "fit3b" ,
  fit4b = "fit4b" ,
  fit5b = "fit5b" ,
  fit6b = "fit6b" 
)

all_mod_nm_df <- data.table::transpose(all_mod_nm)
ctime_df <- data.frame(
  sapply(names(all_mod_nm), function(x) {
    sum(get(x)[["bru_timings"]][["Time"]])
  })
)

ctime_df_ <- data.frame(all_mod_nm_df, ctime_df)

names(ctime_df_) <- NULL

print(
  xtable::xtable(
    ctime_df_
  ),
  file = paste0("figures/model/ctime/", trainset, "_", nm_chess, "ctime_tab.txt")
)


ls_ctime_id <- lapply(
  names(all_mod_nm),
  function(x) get(x)[["bru_timings"]]
)
names(ls_ctime_id) <- names(all_mod_nm)
ls_ctime_id_ <- ls_ctime_id %>%
  bind_rows(.id = "type")


# arranging order
ls_ctime_id_$type <- factor(ls_ctime_id_$type,
                            levels = unique(ls_ctime_id_$type)
)

ggplot(
  data = ls_ctime_id_,
  aes(Time, type, fill = Task)
) +
  geom_bar(position = "stack", stat = "identity") +
  xlab("Time (sec)") +
  ylab("Model")
ggsave(paste0("figures/model/ctime/", trainset, "_", nm_chess, "comp_time_type.png"), dpi = 300,
       width = 6, height = 6 / 1.25
)

rownames(ls_ctime_id_) <- NULL
print(
  xtable::xtable(
    ls_ctime_id_
  ),
  include.rownames = FALSE,
  file = paste0("figures/model/ctime/", trainset, "_", nm_chess, "ls_ctime_tab.txt")
)