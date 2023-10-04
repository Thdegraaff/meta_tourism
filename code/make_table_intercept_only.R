model_path <- here::here("output", "intercept_only.RData")
load(file = model_path)

################## Results

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

######################################## intercept-only models

transformed.m_1 <- ggs(m_0)
boot.m_1 <- filter(transformed.m_1, Iteration >= 1001)
boot.m_1 <- dplyr::select(pivot_wider(boot.m_1, names_from = Parameter, values_from = value), -Iteration, -Chain)
results.m_1 <- ex.results(boot.m_1)

transformed.m_2 <- ggs(m_0_varying_effects)
boot.m_2 <- filter(transformed.m_2, Iteration >= 1001)
boot.m_2 <- dplyr::select(pivot_wider(boot.m_2, names_from = Parameter, values_from = value), -Iteration, -Chain)
results.m_2 <- ex.results(boot.m_2)

transformed.m_3 <- ggs(m_0_me)
boot.m_3 <- filter(transformed.m_3, Iteration >= 1001)
boot.m_3 <- dplyr::select(pivot_wider(boot.m_3, names_from = Parameter, values_from = value), -Iteration, -Chain)
results.m_3 <- ex.results(boot.m_3)

transformed.m_4 <- ggs(m_0_student)
boot.m_4 <- filter(transformed.m_4, Iteration >= 1001)
boot.m_4 <- dplyr::select(pivot_wider(boot.m_4, names_from = Parameter, values_from = value), -Iteration, -Chain)
results.m_4 <- ex.results(boot.m_4)

tr.m_1 <- extract.tr(results.m_1, m_0)
tr.m_2 <- extract.tr(results.m_2, m_0_varying_effects)
tr.m_3 <- extract.tr(results.m_3, m_0_me)
tr.m_4 <- extract.tr(results.m_4, m_0_student)

loo.m_1 <- loo(m_0)
loo.m_2 <- loo(m_0_varying_effects)
loo.m_3 <- loo(m_0_me)
loo.m_4 <- loo(m_0_student)

tr.m_1 <- append.tr(tr.m_1, loo.m_1)
tr.m_2 <- append.tr(tr.m_2, loo.m_2)
tr.m_3 <- append.tr(tr.m_3, loo.m_3)
tr.m_4 <- append.tr(tr.m_4, loo.m_4)

tex_path <- here::here("output/intercept_only_models.tex")

texreg(list(tr.m_1, tr.m_2, tr.m_3, tr.m_4),
       custom.model.names = c("m\\_0\\_A", "m\\_0\\_B", "m\\_0\\_C", "m\\_0\\_D"),
       digits = 2, single.row = TRUE, stars = c(0.01, 0.05, 0.10), 
       dcolumn = TRUE, booktabs = TRUE, longtable = TRUE, use.packages = FALSE,
       omit.coef = "r_study_|lprior",
       file =tex_path)

######################################## pubbias models

model_path <- here::here("output", "pubbias.RData")
load(file = model_path)

transformed.m_1 <- ggs(m_0_student)
boot.m_1 <- filter(transformed.m_1, Iteration >= 1001)
boot.m_1 <- dplyr::select(pivot_wider(boot.m_1, names_from = Parameter, values_from = value), -Iteration, -Chain)
results.m_1 <- ex.results(boot.m_1)

transformed.m_2 <- ggs(m_0_student_a)
boot.m_2 <- filter(transformed.m_2, Iteration >= 1001)
boot.m_2 <- dplyr::select(pivot_wider(boot.m_2, names_from = Parameter, values_from = value), -Iteration, -Chain)
results.m_2 <- ex.results(boot.m_2)

transformed.m_3 <- ggs(m_0_student_b)
boot.m_3 <- filter(transformed.m_3, Iteration >= 1001)
boot.m_3 <- dplyr::select(pivot_wider(boot.m_3, names_from = Parameter, values_from = value), -Iteration, -Chain)
results.m_3 <- ex.results(boot.m_3)

transformed.m_4 <- ggs(m_0_student_c)
boot.m_4 <- filter(transformed.m_4, Iteration >= 1001)
boot.m_4 <- dplyr::select(pivot_wider(boot.m_4, names_from = Parameter, values_from = value), -Iteration, -Chain)
results.m_4 <- ex.results(boot.m_4)

tr.m_1 <- extract.tr(results.m_1, m_0_student)
tr.m_2 <- extract.tr(results.m_2, m_0_student_a)
tr.m_3 <- extract.tr(results.m_3, m_0_student_b)
tr.m_4 <- extract.tr(results.m_4, m_0_student_c)

loo.m_1 <- loo(m_0_student)
loo.m_2 <- loo(m_0_student_a)
loo.m_3 <- loo(m_0_student_b)
loo.m_4 <- loo(m_0_student_c)

tr.m_1 <- append.tr(tr.m_1, loo.m_1)
tr.m_2 <- append.tr(tr.m_2, loo.m_2)
tr.m_3 <- append.tr(tr.m_3, loo.m_3)
tr.m_4 <- append.tr(tr.m_4, loo.m_4)

tex_path <- here::here("output/pubbias_models.tex")

texreg(list(tr.m_1, tr.m_2, tr.m_3, tr.m_4),
       custom.model.names = c("m\\_0\\_A", "m\\_0\\_B", "m\\_0\\_C", "m\\_0\\_D"),
       digits = 2, single.row = TRUE, stars = c(0.01, 0.05, 0.10), 
       dcolumn = TRUE, booktabs = TRUE, longtable = TRUE, use.packages = FALSE,
       omit.coef = "r_study_|lprior",
       file =tex_path)
