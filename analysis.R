# ================================================================
#  FDA DRUG REPURPOSING ANALYSIS
#  Does Drug Repurposing Lead to Faster FDA Approval?
#  Aritra Saha Utsha | MSc Thesis | University of Gothenburg
# ================================================================
#
#  DATA: data/FDA_MSc_Final.xlsx  (run from the repository root)
#
#  
# ================================================================


# ================================================================
# SECTION 0 — LOAD PACKAGES
# If you get "package not found" errors, run this first:
# install.packages(c("readxl","dplyr","tidyr","ggplot2","ggpubr",
#   "scales","MASS","lmtest","sandwich","corrplot","tibble"))
# ================================================================


library(readxl)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(lmtest)
library(sandwich)
library(corrplot)
library(tibble)
library(MASS)     # load MASS last to avoid masking issues

# Fix the select() conflict caused by MASS masking dplyr
library(dplyr)
select <- dplyr::select   # force select to always use dplyr version

# Colour palette
C_REP   <- "#028090"
C_NREP  <- "#0D1B3E"
C_ACC   <- "#02C39A"
C_LIGHT <- "#E0EAF4"
C_GREY  <- "#8FA8BE"
C_RED   <- "#B85042"

cat("All packages loaded.\n")


# ================================================================
# SECTION 1 — LOAD AND PREPARE DATA
# ================================================================

file_path  <- "data/FDA_MSc_Final.xlsx"

sheet_main <- read_excel(file_path, sheet = "FDA_MSc")
sheet1     <- read_excel(file_path, sheet = "Sheet1")

cat(sprintf("FDA_MSc: %d rows | Sheet1: %d rows\n",
            nrow(sheet_main), nrow(sheet1)))

# Merge using base R merge to avoid any dplyr conflict
df <- merge(sheet1,
            sheet_main[, c("ApplNo","ReviewPriority",
                           "MarketingStatusDescription",
                           "Submission_Year")],
            by = "ApplNo", all.x = TRUE)

# Create all variables
df$duration      <- as.numeric(df$`Approval Duration`)
df$Repurposed    <- ifelse(df$`Repurposed (Yes/No)` == "Yes", 1L, 0L)
df$Priority      <- ifelse(df$ReviewPriority == "PRIORITY", 1L, 0L)
df$Discontinued  <- ifelse(df$MarketingStatusDescription == "Discontinued", 1L, 0L)
df$MarketingStatus <- df$MarketingStatusDescription
df$Year          <- df$Submission_Year
df$Repurposed_f  <- factor(df$Repurposed, levels = c(0,1),
                           labels = c("Non-Repurposed","Repurposed"))
df$log_duration  <- suppressWarnings(log(df$duration))

# Analysis dataset
adf        <- df[!is.na(df$duration) & df$duration > 0, ]
rep_group  <- adf$duration[adf$Repurposed == 1]
nrep_group <- adf$duration[adf$Repurposed == 0]

cat(sprintf("Full N=%d | Analysis N=%d | Rep=%d | NonRep=%d\n",
            nrow(df), nrow(adf), length(rep_group), length(nrep_group)))
cat(sprintf("Missing/excluded: %d\n", nrow(df) - nrow(adf)))


# ================================================================
# SECTION 2 — DESCRIPTIVE STATISTICS
# ================================================================

cat("\n=== TABLE 1: Descriptive Statistics ===\n")

desc <- data.frame(
  Group    = c("Full Sample","Non-Repurposed","Repurposed"),
  N        = c(nrow(adf), length(nrep_group), length(rep_group)),
  Mean_d   = round(c(mean(adf$duration), mean(nrep_group), mean(rep_group)), 1),
  Median_d = round(c(median(adf$duration), median(nrep_group), median(rep_group)), 1),
  SD       = round(c(sd(adf$duration), sd(nrep_group), sd(rep_group)), 1),
  Min      = c(min(adf$duration), min(nrep_group), min(rep_group)),
  Max      = c(max(adf$duration), max(nrep_group), max(rep_group)),
  Mean_yr  = round(c(mean(adf$duration), mean(nrep_group), mean(rep_group)) / 365, 2)
)
print(desc)


# ================================================================
# SECTION 3 — OVERDISPERSION
# ================================================================

od_mean  <- mean(adf$duration)
od_var   <- var(adf$duration)
od_ratio <- od_var / od_mean

cat(sprintf("\n=== OVERDISPERSION ===\n"))
cat(sprintf("Mean=%.1f | Variance=%.0f | Ratio=%.1f\n",
            od_mean, od_var, od_ratio))
cat("Ratio >> 1 confirms negative binomial is appropriate.\n")


# ================================================================
# SECTION 4 — CORRELATION MATRIX
# ================================================================

cat("\n=== TABLE 2: Spearman Correlation Matrix ===\n")

corr_data <- data.frame(
  LogDuration    = adf$log_duration,
  Repurposed     = adf$Repurposed,
  PriorityReview = adf$Priority,
  SubmissionYear = adf$Year,
  Discontinued   = adf$Discontinued
)
corr_data   <- na.omit(corr_data)
corr_matrix <- cor(corr_data, method = "spearman")
colnames(corr_matrix) <- rownames(corr_matrix) <- c(
  "Log Duration","Repurposed","Priority Review",
  "Submission Year","Discontinued")
print(round(corr_matrix, 3))


# ================================================================
# SECTION 5 — BIVARIATE TESTS
# ================================================================

cat("\n=== TABLE 3: BIVARIATE TESTS ===\n")

mw_one     <- wilcox.test(rep_group, nrep_group,
                          alternative = "less",      exact = FALSE)
mw_two     <- wilcox.test(rep_group, nrep_group,
                          alternative = "two.sided", exact = FALSE)
grand_med  <- median(adf$duration)
above_med  <- as.integer(adf$duration > grand_med)
ct         <- table(adf$Repurposed, above_med)
chi_res    <- chisq.test(ct)
fisher_res <- fisher.test(ct)
ttest_res  <- t.test(rep_group, nrep_group)

cat(sprintf("Mann-Whitney one-sided  p = %.4f\n", mw_one$p.value))
cat(sprintf("Mann-Whitney two-sided  p = %.4f\n", mw_two$p.value))
cat(sprintf("Chi-squared             p = %.4f\n", chi_res$p.value))
cat(sprintf("Fisher's exact          p = %.4f\n", fisher_res$p.value))
cat(sprintf("T-test (reference)      p = %.4f\n", ttest_res$p.value))
cat("None significant at p < 0.05\n")


# ================================================================
# SECTION 6 — REGRESSION
# ================================================================

cat("\n=== REGRESSION ANALYSIS ===\n")

reg_data <- adf[!is.na(adf$duration) & !is.na(adf$Repurposed) &
                  !is.na(adf$Priority) & !is.na(adf$Year) &
                  !is.na(adf$Discontinued), ]
reg_data$duration_int <- as.integer(round(reg_data$duration))

cat(sprintf("Regression N = %d\n", nrow(reg_data)))

# Negative Binomial models
nb_m1 <- glm.nb(duration_int ~ Repurposed,
                data = reg_data, link = log)
nb_m2 <- glm.nb(duration_int ~ Repurposed + Priority + Year,
                data = reg_data, link = log)
nb_m3 <- glm.nb(duration_int ~ Repurposed + Priority +
                  Year + Discontinued,
                data = reg_data, link = log)
nb_m4 <- glm.nb(duration_int ~ Repurposed * Priority +
                  Year + Discontinued,
                data = reg_data, link = log)

cat("\n--- IRR Model 1 ---\n")
print(round(exp(cbind(IRR=coef(nb_m1), confint(nb_m1))), 4))
cat("\n--- IRR Model 2 ---\n")
print(round(exp(cbind(IRR=coef(nb_m2), confint(nb_m2))), 4))
cat("\n--- IRR Model 3 ---\n")
print(round(exp(cbind(IRR=coef(nb_m3), confint(nb_m3))), 4))
cat("\n--- IRR Model 4 (Interaction H2) ---\n")
print(round(exp(cbind(IRR=coef(nb_m4), confint(nb_m4))), 4))

cat(sprintf("\nAIC: M1=%.1f | M2=%.1f | M3=%.1f\n",
            AIC(nb_m1), AIC(nb_m2), AIC(nb_m3)))

# OLS robustness
ols_m1 <- lm(log_duration ~ Repurposed, data = reg_data)
ols_m2 <- lm(log_duration ~ Repurposed + Priority + Year,
             data = reg_data)
ols_m3 <- lm(log_duration ~ Repurposed + Priority +
               Year + Discontinued,
             data = reg_data)

cat("\n--- OLS M2 (Robust SE) ---\n")
print(coeftest(ols_m2, vcov = vcovHC(ols_m2, "HC3")))


# ================================================================
# SECTION 7 — SUBGROUP ANALYSIS
# ================================================================

cat("\n=== TABLE 5: SUBGROUP ANALYSIS ===\n")

run_sub <- function(label, sub) {
  r  <- sub$duration[sub$Repurposed == 1]
  nr <- sub$duration[sub$Repurposed == 0]
  if (length(r) >= 3 & length(nr) >= 3) {
    wt <- wilcox.test(r, nr, exact = FALSE)
    data.frame(Subgroup    = label,
               N_Rep       = length(r),
               Mean_Rep    = round(mean(r), 0),
               N_NonRep    = length(nr),
               Mean_NonRep = round(mean(nr), 0),
               Diff        = round(mean(r) - mean(nr), 0),
               MW_p        = round(wt$p.value, 4))
  }
}

sub_df <- rbind(
  run_sub("Priority Review",      adf[adf$Priority    == 1, ]),
  run_sub("Standard Review",      adf[adf$Priority    == 0, ]),
  run_sub("Post-PDUFA (1992-99)", adf[adf$Year        >= 1992, ]),
  run_sub("Pre-PDUFA (1990-91)",  adf[adf$Year        <  1992, ]),
  run_sub("Discontinued",         adf[adf$Discontinued == 1, ]),
  run_sub("Prescription",         adf[adf$MarketingStatus == "Prescription", ])
)
print(sub_df)


# ================================================================
# SECTION 8 — FIGURES (display in Plots tab)
# ================================================================

# ---- Figure 1 ------------------------------------------------
year_df <- aggregate(
  cbind(total = Repurposed) ~ Year,
  data = df[!is.na(df$Year), ],
  FUN  = length)
year_df$repurp  <- tapply(df$Repurposed[!is.na(df$Year)],
                          df$Year[!is.na(df$Year)], sum)[as.character(year_df$Year)]
year_df$pct_rep <- year_df$repurp / year_df$total * 100

fig1 <- ggplot(year_df, aes(x = Year)) +
  geom_col(aes(y = total),  fill = C_LIGHT, colour = C_GREY,
           linewidth = 0.6, width = 0.7) +
  geom_col(aes(y = repurp), fill = C_REP, alpha = 0.88, width = 0.7) +
  geom_vline(xintercept = 1992.5, linetype = "dashed",
             colour = C_RED, linewidth = 1.3) +
  annotate("text", x = 1993.1, y = 59, label = "PDUFA\n1992",
           colour = C_RED, size = 3.3, hjust = 0, fontface = "bold") +
  geom_line(aes(y = pct_rep * 0.68),  colour = C_ACC, linewidth = 1.8) +
  geom_point(aes(y = pct_rep * 0.68), colour = C_ACC, size = 3.8,
             fill = "white", shape = 21, stroke = 2) +
  scale_x_continuous(breaks = 1990:1999) +
  scale_y_continuous(
    name = "Number of NDA Submissions", limits = c(0, 70),
    sec.axis = sec_axis(~ . / 0.68, name = "Repurposed Drugs (%)",
                        breaks = seq(0, 80, 20))) +
  labs(title    = "Figure 1: NDA Submissions and Proportion of Repurposed Drugs (1990\u20131999)",
       subtitle = "Bars = total NDAs (teal = repurposed)  |  Line = % repurposed (right axis)",
       x = "Submission Year") +
  theme_classic(base_size = 11) +
  theme(plot.title         = element_text(face = "bold", size = 12),
        plot.subtitle      = element_text(colour = C_GREY, size = 9),
        axis.title.y.right = element_text(colour = C_ACC),
        axis.text.y.right  = element_text(colour = C_ACC),
        panel.grid.major.y = element_line(colour = "#EEEEEE"))
print(fig1)

# ---- Figure 2 ------------------------------------------------
sum_df <- data.frame(
  Repurposed_f = factor(c("Non-Repurposed","Repurposed"),
                        levels = c("Non-Repurposed","Repurposed")),
  Mean   = c(mean(nrep_group), mean(rep_group)),
  Median = c(median(nrep_group), median(rep_group)),
  SE     = c(sd(nrep_group)/sqrt(length(nrep_group)),
             sd(rep_group)/sqrt(length(rep_group)))
)
sum_df$CI_lo <- sum_df$Mean - 1.96 * sum_df$SE
sum_df$CI_hi <- sum_df$Mean + 1.96 * sum_df$SE

p2a <- ggplot(sum_df, aes(x = Repurposed_f, y = Mean,
                          fill = Repurposed_f)) +
  geom_col(width = 0.5, alpha = 0.87) +
  geom_errorbar(aes(ymin = CI_lo, ymax = CI_hi),
                width = 0.12, linewidth = 1.6) +
  geom_text(aes(label = sprintf("%.0f days\n(%.2f years)",
                                Mean, Mean/365)),
            vjust = -0.7, fontface = "bold", size = 3.9) +
  scale_fill_manual(values = c(C_NREP, C_REP)) +
  scale_y_continuous(limits = c(0, 950), labels = comma) +
  labs(title = "Mean Approval Duration\n(95% CI)", x = NULL,
       y = "Approval Duration (Days)") +
  annotate("text", x = 1.5, y = 18,
           label = sprintf("Mann-Whitney (two-sided): p=%.2f | Not significant",
                           mw_two$p.value),
           colour = C_GREY, size = 3, fontface = "italic") +
  theme_classic(base_size = 11) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 11),
        panel.grid.major.y = element_line(colour = "#EEEEEE"))

p2b <- ggplot(sum_df, aes(x = Repurposed_f, y = Median,
                          fill = Repurposed_f)) +
  geom_col(width = 0.5, alpha = 0.87) +
  geom_text(aes(label = sprintf("%.0f days\n(%.2f years)",
                                Median, Median/365)),
            vjust = -0.7, fontface = "bold", size = 3.9) +
  scale_fill_manual(values = c(C_NREP, C_REP)) +
  scale_y_continuous(limits = c(0, 580), labels = comma) +
  labs(title = "Median Approval Duration", x = NULL,
       y = "Approval Duration (Days)") +
  annotate("text", x = 1.5, y = 12,
           label = "Median difference: 39 days (repurposed faster)",
           colour = C_GREY, size = 3, fontface = "italic") +
  theme_classic(base_size = 11) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 11),
        panel.grid.major.y = element_line(colour = "#EEEEEE"))

fig2 <- ggarrange(p2a, p2b, ncol = 2) %>%
  annotate_figure(top = text_grob(
    "Figure 2: Regulatory Approval Duration \u2014 Repurposed vs Non-Repurposed",
    face = "bold", size = 12))
print(fig2)

# ---- Figure 3 ------------------------------------------------
p3a <- ggplot(adf, aes(x = Repurposed_f, y = duration,
                       fill = Repurposed_f)) +
  geom_boxplot(alpha = 0.82, outlier.alpha = 0.3,
               outlier.size = 1.5, width = 0.5) +
  geom_jitter(aes(colour = Repurposed_f),
              width = 0.12, alpha = 0.18, size = 1.2) +
  scale_fill_manual(values   = c(C_NREP, C_REP)) +
  scale_colour_manual(values = c(C_NREP, C_REP)) +
  scale_y_continuous(labels = comma) +
  labs(title = "Distribution of Approval Durations\n(Boxplot with Jitter)",
       x = NULL, y = "Approval Duration (Days)") +
  theme_classic(base_size = 11) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", size = 11),
        panel.grid.major.y = element_line(colour = "#EEEEEE"))

p3b <- ggplot(adf, aes(x = log_duration, fill = Repurposed_f,
                       colour = Repurposed_f)) +
  geom_histogram(aes(y = after_stat(density)), bins = 22,
                 alpha = 0.62, position = "identity") +
  scale_fill_manual(values   = c(C_NREP, C_REP), name = "") +
  scale_colour_manual(values = c(C_NREP, C_REP), name = "") +
  labs(title = "Log-Transformed Distribution\n(Regression Dependent Variable)",
       x = "Log(Approval Duration in Days)", y = "Density") +
  annotate("text", x = 4.1, y = 1.15,
           label = sprintf("Var/Mean = %.0f\n\u2192 Overdispersion confirmed",
                           od_ratio),
           colour = C_GREY, size = 3, fontface = "italic", hjust = 0) +
  theme_classic(base_size = 11) +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 11),
        panel.grid.major.y = element_line(colour = "#EEEEEE"))

fig3 <- ggarrange(p3a, p3b, ncol = 2) %>%
  annotate_figure(top = text_grob(
    "Figure 3: Distribution of Approval Duration by Repurposing Status",
    face = "bold", size = 12))
print(fig3)

# ---- Figure 4 ------------------------------------------------
fig4_data <- data.frame(
  Subgroup    = rep(sub_df$Subgroup, 2),
  Mean        = c(sub_df$Mean_NonRep, sub_df$Mean_Rep),
  Group       = factor(c(rep("Non-Repurposed", nrow(sub_df)),
                         rep("Repurposed",     nrow(sub_df))),
                       levels = c("Non-Repurposed","Repurposed")),
  Mean_Rep    = rep(sub_df$Mean_Rep,    2),
  Mean_NonRep = rep(sub_df$Mean_NonRep, 2),
  MW_p        = rep(sub_df$MW_p, 2)
)

fig4 <- ggplot(fig4_data, aes(x = Subgroup, y = Mean, fill = Group)) +
  geom_col(position = position_dodge(0.72), width = 0.68, alpha = 0.87) +
  geom_text(data = sub_df,
            aes(x = Subgroup,
                y = pmax(Mean_Rep, Mean_NonRep) + 30,
                label = ifelse(MW_p < 0.05,
                               sprintf("p=%.3f*", MW_p),
                               sprintf("p=%.2f",  MW_p))),
            inherit.aes = FALSE, size = 3, colour = "#444444") +
  scale_fill_manual(values = c(C_NREP, C_REP), name = "") +
  scale_y_continuous(labels = comma) +
  labs(title = "Figure 4: Subgroup Analysis \u2014 Mean Approval Duration by Category",
       x = NULL, y = "Mean Approval Duration (Days)") +
  theme_classic(base_size = 11) +
  theme(legend.position    = "bottom",
        plot.title         = element_text(face = "bold", size = 12),
        axis.text.x        = element_text(size = 9.5),
        panel.grid.major.y = element_line(colour = "#EEEEEE"))
print(fig4)

# ---- Figure 5 ------------------------------------------------
corrplot(corr_matrix,
         method = "color", type = "lower",
         addCoef.col = "black", number.cex = 0.85,
         tl.col = "black", tl.srt = 35, tl.cex = 0.95,
         col = colorRampPalette(c(C_NREP, "white", C_REP))(200),
         title = "Figure 5: Spearman Correlation Matrix",
         mar   = c(0, 0, 2.5, 0))

# ---- Figure 6 ------------------------------------------------
make_irr <- function(model, label) {
  coefs <- coef(model)
  cis   <- suppressMessages(confint(model))
  data.frame(Model    = label,
             Variable = names(coefs),
             IRR      = exp(coefs),
             CI_lo    = exp(cis[, 1]),
             CI_hi    = exp(cis[, 2]),
             stringsAsFactors = FALSE)
}

irr_df <- rbind(make_irr(nb_m1, "M1: Baseline"),
                make_irr(nb_m2, "M2: + Controls"),
                make_irr(nb_m3, "M3: Full Model"))

irr_df <- irr_df[irr_df$Variable != "(Intercept)", ]
irr_df$Variable[irr_df$Variable == "Priority"]     <- "Priority Review"
irr_df$Variable[irr_df$Variable == "Year"]         <- "Submission Year"
irr_df$Model <- factor(irr_df$Model,
                       levels = c("M1: Baseline",
                                  "M2: + Controls",
                                  "M3: Full Model"))
irr_df$Highlight <- ifelse(irr_df$Variable == "Repurposed", "yes", "no")

fig6 <- ggplot(irr_df, aes(x = IRR, y = Variable, colour = Highlight)) +
  geom_vline(xintercept = 1, linetype = "dashed",
             colour = C_RED, linewidth = 1.0, alpha = 0.8) +
  geom_point(size = 4) +
  geom_errorbar(aes(xmin = CI_lo, xmax = CI_hi),
                width = 0.22, linewidth = 1.4,
                orientation = "y") +
  geom_text(aes(label = sprintf("%.3f", IRR)),
            vjust = -1.0, size = 3.3) +
  scale_colour_manual(values = c("yes" = C_REP, "no" = C_GREY)) +
  facet_wrap(~ Model, ncol = 3) +
  labs(title    = "Figure 6: Negative Binomial Regression \u2014 Incidence Rate Ratios (95% CI)",
       subtitle = "IRR < 1 = shorter duration  |  IRR > 1 = longer  |  Red dashed = no effect",
       x = "Incidence Rate Ratio", y = NULL) +
  theme_classic(base_size = 11) +
  theme(legend.position    = "none",
        plot.title         = element_text(face = "bold", size = 12),
        plot.subtitle      = element_text(colour = C_GREY, size = 9),
        strip.background   = element_rect(fill = C_NREP, colour = NA),
        strip.text         = element_text(colour = "white", face = "bold"),
        panel.grid.major.x = element_line(colour = "#EEEEEE"))
print(fig6)


# ================================================================
# SECTION 9 — FINAL SUMMARY
# ================================================================

cat("\n=======================================================\n")
cat("              FINAL RESULTS SUMMARY\n")
cat("=======================================================\n")
cat(sprintf("Full N=289 | Analysis N=%d\n", nrow(adf)))
cat(sprintf("Repurposed:     N=%d mean=%.0fd (%.2fyr) median=%.0fd\n",
            length(rep_group), mean(rep_group),
            mean(rep_group)/365, median(rep_group)))
cat(sprintf("Non-repurposed: N=%d mean=%.0fd (%.2fyr) median=%.0fd\n",
            length(nrep_group), mean(nrep_group),
            mean(nrep_group)/365, median(nrep_group)))
cat(sprintf("Mean diff: %.0fd | Median diff: %.0fd (repurposed faster)\n",
            mean(nrep_group) - mean(rep_group),
            median(nrep_group) - median(rep_group)))
cat(sprintf("\nMW two-sided    p = %.4f  [NOT significant]\n", mw_two$p.value))
cat(sprintf("Chi-squared     p = %.4f  [NOT significant]\n", chi_res$p.value))
cat(sprintf("Fisher exact    p = %.4f  [NOT significant]\n", fisher_res$p.value))
cat(sprintf("\nNB M1 Repurposed IRR = %.4f (%.1f%% shorter, not significant)\n",
            exp(coef(nb_m1)["Repurposed"]),
            (1 - exp(coef(nb_m1)["Repurposed"])) * 100))
cat(sprintf("NB M2 Repurposed IRR = %.4f (not significant)\n",
            exp(coef(nb_m2)["Repurposed"])))
cat(sprintf("NB M2 Year IRR       = %.4f (PDUFA effect, highly significant)\n",
            exp(coef(nb_m2)["Year"])))
cat(sprintf("Overdispersion ratio = %.0f --> NB model confirmed\n", od_ratio))
cat("=======================================================\n")
cat("All 6 figures displayed in the Plots tab.\n")
cat("Use the arrows in the Plots tab to scroll between them.\n")
cat("=======================================================\n")
par(mfrow = c(1,1))
grid.newpage <- function() {}
print(p2a)


# ================================================================
#  TABLE 4 — Regression Results
#  Run this AFTER Section 6 of the main analysis script
#  (nb_m1, nb_m2, nb_m3, ols_m1, ols_m2, ols_m3 must be in memory)
# ================================================================

library(flextable)
library(officer)

C_NREP  <- "#0D1B3E"
C_LIGHT <- "#E0EAF4"

# ---- Helper to extract NB coefficient -------------------------
get_nb <- function(model, vname) {
  cf <- tryCatch(coef(summary(model)), error = function(e) NULL)
  if (is.null(cf) || !vname %in% rownames(cf)) return("—")
  est <- exp(cf[vname, "Estimate"])
  pv  <- cf[vname, "Pr(>|z|)"]
  se  <- cf[vname, "Std. Error"]
  sig <- ifelse(pv < 0.001, "***",
                ifelse(pv < 0.01,  "**",
                       ifelse(pv < 0.05,  "*", "")))
  sprintf("%.3f%s\n(%.3f)", est, sig, se)
}

# ---- Helper to extract OLS coefficient ------------------------
get_ols <- function(model, vname) {
  cf <- tryCatch(coef(summary(model)), error = function(e) NULL)
  if (is.null(cf) || !vname %in% rownames(cf)) return("—")
  est <- cf[vname, "Estimate"]
  pv  <- cf[vname, "Pr(>|t|)"]
  se  <- cf[vname, "Std. Error"]
  sig <- ifelse(pv < 0.001, "***",
                ifelse(pv < 0.01,  "**",
                       ifelse(pv < 0.05,  "*", "")))
  sprintf("%.3f%s\n(%.3f)", est, sig, se)
}

# ---- Build table rows -----------------------------------------
make_row <- function(varname, display_name) {
  data.frame(
    Variable = display_name,
    NB_M1    = get_nb(nb_m1,  varname),
    NB_M2    = get_nb(nb_m2,  varname),
    NB_M3    = get_nb(nb_m3,  varname),
    OLS_M1   = get_ols(ols_m1, varname),
    OLS_M2   = get_ols(ols_m2, varname),
    OLS_M3   = get_ols(ols_m3, varname),
    stringsAsFactors = FALSE
  )
}

t4 <- rbind(
  make_row("Repurposed",   "Repurposed (1=Yes)"),
  make_row("Priority",     "Priority Review (1=Yes)"),
  make_row("Year",         "Submission Year"),
  make_row("Discontinued", "Discontinued (1=Yes)")
)

# ---- Add summary rows -----------------------------------------
t4 <- rbind(t4,
            data.frame(Variable="N (usable)",
                       NB_M1=as.character(nrow(reg_data)),
                       NB_M2=as.character(nrow(reg_data)),
                       NB_M3=as.character(nrow(reg_data)),
                       OLS_M1=as.character(nrow(reg_data)),
                       OLS_M2=as.character(nrow(reg_data)),
                       OLS_M3=as.character(nrow(reg_data)),
                       stringsAsFactors=FALSE),
            data.frame(Variable="Overdispersion (\u03b1)",
                       NB_M1=sprintf("%.3f", nb_m1$theta),
                       NB_M2=sprintf("%.3f", nb_m2$theta),
                       NB_M3=sprintf("%.3f", nb_m3$theta),
                       OLS_M1="\u2014", OLS_M2="\u2014", OLS_M3="\u2014",
                       stringsAsFactors=FALSE)
)

# ---- Column headers -------------------------------------------
colnames(t4) <- c("Variable",
                  "NB M1\nIRR (SE)",
                  "NB M2\nIRR (SE)",
                  "NB M3\nIRR (SE)",
                  "OLS M1\n\u03b2 (SE)",
                  "OLS M2\n\u03b2 (SE)",
                  "OLS M3\n\u03b2 (SE)")

# ---- Build flextable ------------------------------------------
ft4 <- flextable(t4) %>%
  bold(part = "header") %>%
  bg(part = "header", bg = C_NREP) %>%
  color(part = "header", color = "white") %>%
  bg(i = c(1, 3, 5, 7), bg = C_LIGHT) %>%
  bg(i = c(2, 4, 6),    bg = "white") %>%
  bold(j = 1) %>%
  align(j = 2:7, align = "center", part = "all") %>%
  align(j = 1,   align = "left",   part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  fontsize(size = 10.5, part = "header") %>%
  line_spacing(space = 1.2, part = "body") %>%
  autofit()

# ---- Save to Word ---------------------------------------------
doc4 <- read_docx() %>%
  body_add_par(
    paste0("Notes: NB = Negative Binomial (primary model). ",
           "IRR = Incidence Rate Ratio. ",
           "OLS \u03b2 = coefficient on log(Approval Duration). ",
           "SE in parentheses. ",
           "* p<0.05  ** p<0.01  *** p<0.001"),
    style = "Normal") %>%
  body_add_par("") %>%
  body_add_flextable(ft4)

print(doc4, target = "output/tables/Table4_Regression.docx")
cat("Done. Table4_Regression.docx saved to your working directory.\n")
