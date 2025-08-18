DPHpDPDplot = function(selected_data){ 
  
  selected_data=selected_data[order(selected_data$datetime),]
  
  selected_data$Year=year(selected_data$datetime)
  selected_data$Mon=month(selected_data$datetime)
  
  selected_data$DPH=1
  
  
  
  DPD_all = selected_data %>%
    group_by(as.Date(datetime), Group, Mon) %>%
    tally(DPH)
  
  DPD_all$posDay = ifelse(DPD_all$n>=1,1,0)
  colnames(DPD_all)[1]='Date'
  #DPD$Day=as.character(DPD$Day)
  
  counts=as.data.frame(table(DPD_all$Group, DPD_all$Mon))
  names(counts)[1:2]=c('Group', 'Mon')
  posDays=DPD_all %>%
    group_by(Group, Mon) %>%
    tally(posDay)
  
  perDays = merge(counts, posDays,by=c('Group', 'Mon'))
  names(perDays)[4]='DPC'
  
  
  DPD_all$Year=year(DPD_all$Date)
  DPD_all$Mon=as.factor(DPD_all$Mon)
  DPD_all$YM =with(DPD_all, format(Date, "%Y-%m"))
  DPD_all=DPD_all[order(DPD_all$Date),]
  DPD_all$YM=as.factor(DPD_all$YM)
  
  # Calculate the mean number of DPH per day by station and month.
  DPMo=DPD_all %>% 
    group_by(Group, YM) %>% 
    dplyr::summarize(mean = mean(n),
                     uci = CI(n)[1],
                     lci = CI(n)[3]) %>%
    mutate(YM=YM %>% as.factor(),Station=Group %>% as.factor()) 
  
  
  lvls <- expand.grid(lapply(DPMo[, c('Station', 'YM')], levels))
  DPMo_full=merge(DPMo, lvls, all=TRUE)
  
  
  d2a=ggplot(DPMo_full,aes(x = YM, y = mean, color = Station, group=Station)) +
    geom_line(linewidth=1) +
    geom_errorbar(aes(ymin = lci, ymax = uci), width=0.3,position = 'dodge') +
    #scale_colour_manual(values=cbbPalette)+
    theme_minimal() +
    ylab('') +
    xlab('Year-Month') +
    theme(legend.position = 'right') +
    theme(axis.text.x = element_text(angle = 70, vjust = 0.5, hjust=0.3)) +
    theme(text=element_text(size=10)) + 
    xlab('Year-Month')
  
  
  DPMonly=DPD_all %>% 
    group_by(Mon) %>% 
    dplyr::summarize(mean = mean(n),
                     uci = CI(n)[1],
                     lci = CI(n)[3]) %>%
    mutate(Month=Mon %>% as.factor())
  
  
  d2b=ggplot(DPMonly,aes(x = Month, y = mean, group=1)) +
    geom_line( size=1) +
    geom_errorbar(aes(ymin = lci, ymax = uci), width=0.3) +
    scale_y_continuous(
      labels = scales::number_format(accuracy = 0.01)) +
    theme_minimal() +
    ylab('') +
    xlab('Month') +
    theme(text=element_text(size=10))
  
  
  
  d2=ggarrange(d2b, d2a, nrow = 2,heights = c(0.55,0.45))
  
  
  d2_fin =annotate_figure(d2,left = text_grob("Mean Number (95% CI) of DPH per day", color = "black", rot = 90))
  
  
  return(d2_fin)
}
