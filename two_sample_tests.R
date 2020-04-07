# função que calcula Shapiro-Wilk, Levene, Test T e Wilcoxon
two_sample_tests <- function(dataset, factor, s=0.05){
  require(dplyr)
  require(car)
  shapiro_result = c()
  levene_result = c()
  ttest_result = c()
  wilcox_result = c()
  factors = levels(dataset[,factor])
  samples_names = select(dataset, -all_of(factor)) %>% names()
  for(sample_ in samples_names){
    shapiro_p = c()
    
    for(f in factors){
      shapiro_data = filter(dataset, !!sym(factor) == f) %>%
        select(all_of(sample_)) %>%
        pull()
      shapiro_p = rbind(shapiro_p, shapiro.test(shapiro_data)$p.value)
    }
    shapiro_result = cbind(shapiro_result,shapiro_p)
    # variável dependente ~ variável independente
    f = as.formula(paste(sample_,factor,sep="~"))
    levene_p = leveneTest(f, data=dataset)$Pr[1]
    levene_result = cbind(levene_result,levene_p)
    # faz o teste paramétrico de duas amostras
    if(length(shapiro_p) == 2){
      if(!F %in% (shapiro_p > s) && levene_p > s){
        # teste T
        ttest_result = cbind(ttest_result,t.test(f,dataset)$p.value)
        wilcox_result = cbind(wilcox_result,NA)
      }else{
        # teste de Mann-Witney
        wilcox_result = cbind(wilcox_result,wilcox.test(f,dataset)$p.value)
        ttest_result = cbind(ttest_result,NA)
      } 
    }
  }
  rownames(shapiro_result) <- factors
  colnames(shapiro_result) <- colnames(levene_result) <- colnames(ttest_result) <- colnames(wilcox_result) <- samples_names
  rownames(levene_result) <- rownames(ttest_result) <- rownames(wilcox_result) <- "p-value"
  
  list(
    shapiro_wilk = shapiro_result,
    levene = levene_result,
    t_test = ttest_result,
    wilcoxon = wilcox_result
  )
}
