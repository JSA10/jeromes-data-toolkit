#https://cran.r-project.org/web/packages/ChannelAttribution/ChannelAttribution.pdf

install.packages("ChannelAttribution")
library(ChannelAttribution)
data(PathData)


auto_markov_model(Data, "path", "total_conversions", "total_null")
# needs res


res=choose_order(Data, var_path="path", var_conv="total_conversions",
                 var_null="total_null")

#plot auc and penalized auc
plot(res$auc$order,res$auc$auc,type="l",xlab="order",ylab="pauc",main="AUC")
lines(res$auc$order,res$auc$pauc,col="red")
legend("right", legend=c("auc","penalized auc"),
       col=c("black","red"),lty=1)


# three heuristic models available
var_path <- "path"
var_conv <- "total_conversions"

heuristic_models(Data, var_path, var_conv, var_value=NULL, sep=">")
# first touch, last touch and linear touch



markov_model(Data, var_path, var_conv, var_value=NULL, var_null=NULL,
             order=1, nsim=NULL, max_step=NULL, out_more=FALSE, sep=">",
             seed=NULL)
# changing to 4 took some extra time to run



