# =========================
# Random Forest
# =========================

pkgs <- c("randomForest","caret","PRROC","pROC","caTools")
to_install <- pkgs[!pkgs %in% installed.packages()[,"Package"]]
if(length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")

library(randomForest)
library(caret)
library(PRROC)
library(pROC)
library(caTools)

set.seed(42)

# 1) Load
df <- read.csv("preprocessed.csv")
stopifnot(all(c("Height","Width","Aspect_ratio","Local","Is_Ad") %in% names(df)))

# Make factor with positive level "1"
df$Is_Ad <- factor(df$Is_Ad, levels = c(0,1), labels = c("0","1"))

# 2) Stratified split: 60% train, 20% val, 20% test
idx_train <- createDataPartition(df$Is_Ad, p = 0.6, list = FALSE)
train <- df[idx_train,]
tmp   <- df[-idx_train, ]
idx_val <- createDataPartition(tmp$Is_Ad, p = 0.5, list = FALSE)
val   <- tmp[idx_val, ]
test  <- tmp[-idx_val, ]

cat(sprintf("Split -> train=%d, val=%d, test=%d\n", nrow(train), nrow(val), nrow(test)))

# 3) Helper metrics ----------------------------------------------------------
metrics_report <- function(y_true, prob, thr){
  pred <- ifelse(prob >= thr, "1", "0")
  cm <- table(Actual = y_true, Pred = factor(pred, levels=c("0","1")))
  TP <- cm["1","1"]; FP <- cm["0","1"]; TN <- cm["0","0"]; FN <- cm["1","0"]
  precision <- ifelse(TP+FP==0, 0, TP/(TP+FP))
  recall    <- ifelse(TP+FN==0, 0, TP/(TP+FN))
  f1        <- ifelse(precision+recall==0, 0, 2*precision*recall/(precision+recall))
  roc_auc   <- as.numeric(roc(y_true, prob, quiet=TRUE)$auc)
  pr_auc    <- pr.curve(scores.class0 = prob[y_true=="1"],
                        scores.class1 = prob[y_true=="0"], curve = FALSE)$auc.integral
  list(cm=cm, precision=precision, recall=recall, f1=f1, roc_auc=roc_auc, pr_auc=pr_auc, thr=thr)
}

# Tìm ngưỡng nhỏ nhất đạt precision >= target; nếu không đạt, dùng ngưỡng max F1
find_threshold <- function(y_true, prob, precision_target = 0.97){
  ord <- order(prob, decreasing = TRUE)
  y <- (y_true[ord] == "1")
  tp <- cumsum(y)
  fp <- cumsum(!y)
  precision <- tp / pmax(tp+fp, 1)
  recall <- tp / sum(y)
  thr_candidates <- prob[ord]
  # ngưỡng tối thiểu đạt Precision >= target
  ok <- which(precision >= precision_target)
  if(length(ok) > 0){
    thr <- min(thr_candidates[ok])
    return(list(thr=thr, mode=sprintf("Precision>=%.2f", precision_target)))
  } else {
    # F1 max
    f1 <- 2*precision*recall/pmax(precision+recall, 1e-12)
    j <- which.max(f1)
    return(list(thr=thr_candidates[j], mode="MaxF1"))
  }
}

# 4) Grid tìm class weights & mtry trên VALIDATION --------------------------
Xform <- as.formula(Is_Ad ~ Height + Width + Aspect_ratio + Local)

mtry_grid <- c(2,3,4)                   # với 4 đặc trưng
w_grid    <- c(1, 2, 3, 4)              # trọng số cho lớp "1" (ad)
ntree_val <- 500

best <- NULL

for (m in mtry_grid){
  for (w in w_grid){
    rf <- randomForest(
      Xform, data = train,
      ntree = ntree_val,
      mtry  = m,
      classwt = c("0"=1, "1"=w),        # tăng chi phí cho FN
      importance = TRUE
    )
    prob_val <- predict(rf, val, type="prob")[, "1"]
    thr_obj  <- find_threshold(val$Is_Ad, prob_val, precision_target = 0.97)
    rep      <- metrics_report(val$Is_Ad, prob_val, thr_obj$thr)
    score    <- rep$pr_auc                 # ưu tiên PR-AUC trên val
    tag      <- list(mtry=m, w=w, thr=thr_obj$thr, mode=thr_obj$mode,
                     pr_auc=rep$pr_auc, roc_auc=rep$roc_auc,
                     precision=rep$precision, recall=rep$recall, f1=rep$f1, model=rf)
    if (is.null(best) || score > best$pr_auc) best <- tag
  }
}

cat("\n=== Best setting on VALIDATION ===\n")
print(best[c("mtry","w","mode","thr","pr_auc","roc_auc","precision","recall","f1")])

# 5) Đánh giá trên TEST với tham số & ngưỡng đã chốt ------------------------
rf_final <- best$model
prob_test <- predict(rf_final, test, type="prob")[, "1"]
rep_test  <- metrics_report(test$Is_Ad, prob_test, best$thr)

cat("\n=== TEST METRICS (fixed params & threshold) ===\n")
print(rep_test[c("roc_auc","pr_auc","precision","recall","f1","thr")])
cat("\nConfusion matrix (TEST):\n"); print(rep_test$cm)

# 6) Quan trọng đặc trưng
cat("\nVariable Importance (MeanDecreaseGini):\n")
print(importance(rf_final)[, "MeanDecreaseGini", drop=FALSE])
