load("./data/data.Rda")
load("./output/full_models.RData")
# load("./output/sensitivity_models.RData")
studies <- read_excel("./data/data_thomas.xlsx", sheet = "Studies")

data %>% count(whs)

################# Summary statistics

studies <- studies %>%
    select(study_id, authors, year) %>%
    mutate(study_id = as.integer(study_id)) %>%
    mutate(Year = paste0("(", year, ")")) %>%
    unite(authors, authors:year, sep = " ")

data_sum <- left_join(data, studies, by = "study_id")

data_summary <- data_sum %>%
    group_by(authors, study_id) %>%
    summarise(
        estimates = n(),
        mean = mean(dist),
        min = min(dist), 
        max = max(dist)
    )

kable(data_summary, "latex", digits = 2)

## Compute p-values
twosidep <- function(df){
    nvar <- ncol(df)
    p <- data.frame(matrix(NA, nrow=1, ncol=nvar))
    names(p) <- names(df)
    for(i in 1:nvar){
        data <- arrange_(df, sym(colnames(df)[i]))
        p1<-sum(df[,i]>0,na.rm=TRUE)/sum(!is.na(df[,i])) 
        p2<-sum(df[,i]<0,na.rm=TRUE)/sum(!is.na(df[,i])) 
        p[1,i]<-min(p1,p2)*2  
    }
    return(p)
}

# Extract statistics from posterior probability distributions
ex.results <- function(boot.data){
    results <- data.frame("term" = colnames(boot.data),
                          "estimate" = colMeans(boot.data, na.rm = TRUE),
                          "std.error" = apply(boot.data,2,sd,na.rm=TRUE),
                          "statistic" = colMeans(boot.data, na.rm = TRUE)/apply(boot.data,2,sd,na.rm=TRUE),
                          "p.value" = t(twosidep(boot.data)))
    return(results)
}

# Create main tex-reg objects
extract.tr <- function(results, model){
    names <- row.names(results)
    names[1] <- "Mean effect"
    tr <- createTexreg(coef.names = names,
                       coef = results$estimate,
                       se = results$std.error,
                       pvalues = results$p.value,
                       gof.names = c( "looic","Model weights", "Rsq.", "Num.\\ obs."),
                       gof = c(0,0, round(performance::r2(model)$R2_Bayes,3), round(nrow(model$data))))
    return(tr)
}

extract.tr.lmer <- function(results,model){
    tr <- createTexreg(coef.names = results[,1],
                       coef = results$estimate,
                       se = results$std.error,
                       pvalues = results$p.value,
                       gof.names = c("Rsq.","Num.\\ obs."),
                       gof = c(r.squaredGLMM(model)[1],nrow(model@frame)))
    return(tr)
}

# Append out-of-sample model statistics to tex-reg objects
append.tr <- function(tr.object, loo.object){
    #tr.object@gof[1] <- round(loo.object$estimates[rownames(loo.object$estimates) == "elpd_loo","Estimate"],1)
    #tr.object@gof[2] <- round(loo.object$estimates[rownames(loo.object$estimates) == "p_loo","Estimate"],1)
    tr.object@gof[1] <- round(loo.object$estimates[rownames(loo.object$estimates) == "looic","Estimate"],1)
    return(tr.object)
}

######################################## Full models

transformed.m_1 <- ggs(m_1)
boot.m_1 <- filter(transformed.m_1, Iteration >= 1001)
boot.m_1 <- dplyr::select(pivot_wider(boot.m_1, names_from = Parameter, values_from = value), -Iteration, -Chain)
results.m_1 <- ex.results(boot.m_1)

transformed.m_2 <- ggs(m_2)
boot.m_2 <- filter(transformed.m_2, Iteration >= 1001)
boot.m_2 <- dplyr::select(pivot_wider(boot.m_2, names_from = Parameter, values_from = value), -Iteration, -Chain)
results.m_2 <- ex.results(boot.m_2)

transformed.m_3 <- ggs(m_3)
boot.m_3 <- filter(transformed.m_3, Iteration >= 1001)
boot.m_3 <- dplyr::select(pivot_wider(boot.m_3, names_from = Parameter, values_from = value), -Iteration, -Chain)
results.m_3 <- ex.results(boot.m_3)

transformed.m_4 <- ggs(m_4)
boot.m_4 <- filter(transformed.m_4, Iteration >= 1001)
boot.m_4 <- dplyr::select(pivot_wider(boot.m_4, names_from = Parameter, values_from = value), -Iteration, -Chain)
results.m_4 <- ex.results(boot.m_4)

tr.m_1 <- extract.tr(results.m_1, m_1)
tr.m_2 <- extract.tr(results.m_2, m_2)
tr.m_3 <- extract.tr(results.m_3, m_3)
tr.m_4 <- extract.tr(results.m_4, m_4)

loo.m_1 <- loo(m_1)
loo.m_2 <- loo(m_2)
loo.m_3 <- loo(m_3)
loo.m_4 <- loo(m_4)

tr.m_1 <- append.tr(tr.m_1, loo.m_1)
tr.m_2 <- append.tr(tr.m_2, loo.m_2)
tr.m_3 <- append.tr(tr.m_3, loo.m_3)
tr.m_4 <- append.tr(tr.m_4, loo.m_4)

tex_path <- here::here("output/full_models.tex")

texreg(list(tr.m_1, tr.m_2, tr.m_3, tr.m_4),
       custom.model.names = c("m\\_1", "m\\_2", "m\\_3", "m\\_4"),
       digits = 2, single.row = TRUE, stars = c(0.01, 0.05, 0.10), 
       dcolumn = TRUE, booktabs = TRUE, longtable = TRUE, use.packages = FALSE,
       omit.coef = "r_study_|lprior",
       file =tex_path)

######################################## Sensitivity models

# transformed.m_4_pub <- ggs(m_4_pub)
# boot.m_4_pub<- filter(transformed.m_4_pub, Iteration >= 1001)
# boot.m_4_pub <- dplyr::select(pivot_wider(boot.m_4_pub, names_from = Parameter, values_from = value), -Iteration, -Chain)
# results.m_4_pub <- ex.results(boot.m_4_pub)
# 
# transformed.m_4_outlier <- ggs(m_4_outlier )
# boot.m_4_outlier  <- filter(transformed.m_4_outlier , Iteration >= 1001)
# boot.m_4_outlier  <- dplyr::select(pivot_wider(boot.m_4_outlier, names_from = Parameter, values_from = value), -Iteration, -Chain)
# results.m_4_outlier  <- ex.results(boot.m_4_outlier )
# 
# transformed.m_4_nipriors <- ggs(m_4_nipriors)
# boot.m_4_nipriors <- filter(transformed.m_4_nipriors, Iteration >= 1001)
# boot.m_4_nipriors <- dplyr::select(pivot_wider(boot.m_4_nipriors, names_from = Parameter, values_from = value), -Iteration, -Chain)
# results.m_4_nipriors <- ex.results(boot.m_4_nipriors)
# 
# transformed.m_4_time <- ggs(m_4_time)
# boot.m_4_time <- filter(transformed.m_4_time, Iteration >= 1001)
# boot.m_4_time <- dplyr::select(pivot_wider(boot.m_4_time, names_from = Parameter, values_from = value), -Iteration, -Chain)
# results.m_4_time <- ex.results(boot.m_4_time)
# 
# tr.m_4_pub <- extract.tr(results.m_4_pub, m_4_pub)
# tr.m_4_outlier <- extract.tr(results.m_4_outlier, m_4_outlier )
# tr.m_4_nipriors <- extract.tr(results.m_4_nipriors, m_4_nipriors)
# tr.m_4_time <- extract.tr(results.m_4_time, m_4_time)
# 
# loo.m_4_pub <- loo(m_4_pub)
# loo.m_4_outlier <- loo(m_4_outlier )
# loo.m_4_nipriors <- loo(m_4_nipriors)
# loo.m_4_time <- loo(m_4_time)
# 
# tr.m_4_pub<- append.tr(tr.m_4_pub, loo.m_4_pub)
# tr.m_4_outlier  <- append.tr(tr.m_4_outlier , loo.m_2)
# tr.m_4_nipriors <- append.tr(tr.m_4_nipriors, loo.m_4_nipriors)
# tr.m_4_time <- append.tr(tr.m_4_time, loo.m_4_time)
# 
# model_weights(m_1, m_2, m_3, m_4, m_5, weights = "loo")
# 
# tex_path <- here::here("output/sensitivity_models.tex")
# 
# texreg(list(tr.m_4_pub, tr.m_4_outlier , tr.m_4_nipriors, tr.m_4_time),
#        custom.model.names = c("m_4a", "m_4b", "m_4c", "m_4d"),
#        digits = 3, single.row = FALSE, stars = c(0.01, 0.05, 0.10), 
#        dcolumn = TRUE, booktabs = TRUE, longtable = TRUE, use.packages = FALSE,
#        file =tex_path)