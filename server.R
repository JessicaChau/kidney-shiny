library(shiny)
library(GEOquery)
library(tidyverse)
library(limma)

load("gse.RData")

rejection_status = gse[,ncol(gse)]
df = t(gse[,-ncol(gse)])

groupname = factor(rejection_status)
design = model.matrix(~ groupname + 0)
fit = lmFit(df, design)
cont.matrix = makeContrasts(groupnameYes-groupnameNo, levels=design)
fit2 = contrasts.fit(fit, cont.matrix)
fit2 = eBayes(fit2)
tT = topTable(fit2, number=nrow(fit2), genelist=rownames(df))
# ind = rownames(tT[tT$P.Value < 0.05,])

# Define server logic
shinyServer(function(input, output) {

    #output$knn = renderUI({
    #    sliderInput("kval", "Value of K", 
    #                min = 1, max = 20, step = 1, value = 5)
    #})
    
    data = reactive({
        
        num = input$feature
        ind = rownames(tT[1:num,])
        
        if(is.null(input$classifier)){
            return(0)
        }
        
        svm = 1 %in% input$classifier
        knn = 2 %in% input$classifier
        rf = 3 %in% input$classifier
        
        set.seed(1234)
        X = as.matrix(t(df[ind,]))
        y = rejection_status
        
        cvK = 5  # number of CV folds
        cv_50acc5_knn = cv_50acc5_svm = cv_50acc5_rf = c()
        cv_50fp5_knn = cv_50fp5_svm = cv_50fp5_rf = c()
        
        n_sim = 10 ## number of repeats
        for (i in 1:n_sim) {
            
            cvSets = cvTools::cvFolds(nrow(X), cvK)  # permute all the data, into 5 folds
            cv_acc_knn = cv_acc_svm = cv_acc_rf = c()
            cv_fp_knn = cv_fp_svm = cv_fp_rf = c()
            
            for (j in 1:cvK) {
                test_id = cvSets$subsets[cvSets$which == j]
                X_test = X[test_id, ]
                X_train = X[-test_id, ]
                y_test = y[test_id]
                y_train = y[-test_id]
                
                ## KNN
                if (knn) {
                    fit5 = class::knn(train = X_train, test = X_test, cl = y_train, k = input$kval)
                    cv_acc_knn[j] = table(fit5, y_test) %>% diag %>% sum %>% `/`(length(y_test))
                    cv_fp_knn[j] = table(fit5, y_test)[1, 2]/sum(table(fit5, y_test)[,2])
                }
                
                ## SVM
                if (svm) {
                    svm_res <- e1071::svm(x = X_train, y = as.factor(y_train))
                    fit <- predict(svm_res, X_test)
                    cv_acc_svm[j] = table(fit, y_test) %>% diag %>% sum %>% `/`(length(y_test))
                    cv_fp_svm[j] = table(fit, y_test)[1, 2]/sum(table(fit, y_test)[,2])
                }
                
                ## RandomForest
                if (rf) {
                    rf_res <- randomForest::randomForest(x = X_train, y = as.factor(y_train))
                    fit <- predict(rf_res, X_test)
                    cv_acc_rf[j] = table(fit, y_test) %>% diag %>% sum %>% `/`(length(y_test))
                    cv_fp_rf[j] = table(fit, y_test)[1, 2]/sum(table(fit, y_test)[,2])
                }
                
            }
            if (knn) {
                cv_50acc5_knn <- append(cv_50acc5_knn, mean(cv_acc_knn))
                cv_50fp5_knn <- append(cv_50fp5_knn, mean(cv_fp_knn))
            }
            if (svm) {
                cv_50acc5_svm <- append(cv_50acc5_svm, mean(cv_acc_svm))
                cv_50fp5_svm <- append(cv_50fp5_svm, mean(cv_fp_svm))
            }
            if (rf) {
                cv_50acc5_rf <- append(cv_50acc5_rf, mean(cv_acc_rf))
                cv_50fp5_rf <- append(cv_50fp5_rf, mean(cv_fp_rf))
            }
        }
        
        if (svm & rf & knn) {
            c(list(SVM = cv_50acc5_svm, KNN = cv_50acc5_knn, RF= cv_50acc5_rf), 
              list(SVM = cv_50fp5_svm, KNN = cv_50fp5_knn, RF= cv_50fp5_rf))
        } else if (svm & knn) {
            c(list(SVM = cv_50acc5_svm, KNN = cv_50acc5_knn), 
              list(SVM = cv_50fp5_svm, KNN = cv_50fp5_knn))
        } else if (svm & rf) {
            c(list(SVM = cv_50acc5_svm, RF= cv_50acc5_rf), 
              list(SVM = cv_50fp5_svm, RF= cv_50fp5_rf))
        } else if (knn & rf) {
            c(list(KNN = cv_50acc5_knn, RF= cv_50acc5_rf), 
              list(KNN = cv_50fp5_knn, RF= cv_50fp5_rf))
        } else if (svm) {
            c(list(SVM = cv_50acc5_svm), 
              list(SVM = cv_50fp5_svm))
        } else if (knn) {
            c(list(KNN = cv_50acc5_knn), 
              list(KNN = cv_50fp5_knn))
        } else if (rf) {
            c(list(RF= cv_50acc5_rf), 
              list(RF= cv_50fp5_rf))
        }
        
    })
    
    output$plot_acc = renderPlot({
        boxplot(data()[1:(length(data())/2)], ylim = c(0.65, 0.8), ylab = "Accuracy", xlab = "Classifier", main = "Accuracy with differnt classifier")
    })
    
    output$plot_fp = renderPlot({
        boxplot(data()[(length(data())/2+1):length(data())], ylab = "Type I Error", xlab = "Classifier", main = "Type I Error with differnt classifier")
    })
    
    
    
})
