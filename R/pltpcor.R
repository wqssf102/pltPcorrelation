#' @title pltPcorrelation
#'
#' @author Qiusheng WU
#' @return pltpcor
#' @export
#' @examples
#' library(pltPcorrelation)
#' data(mydt)
#' res <- cal_pcor(pcor_dt =mydt)
#' pltpcor(res,high = "red",low = "blue",mid = "gray")

pltpcor <- function(obj=obj,
                    low = 'green', high = 'red', mid = 'blue',...){
   ggplot() +
    geom_point(data=obj, aes(x =  var1 , y = var2,fill=Correlation),shape=22,color="white",size=6)+
    geom_text(data=obj, aes(x =  var1 , y = var2,label=p_sig),size=4)+
    scale_fill_gradient2('Correlation',
                         low = low, high = high, mid = mid)+
    labs(y="")+ theme_gray()+
    theme_classic() +labs(x="",y="")+ theme(axis.text.x = element_text(angle = 90))+
    theme(axis.ticks = element_blank(),
          axis.line = element_blank())+
 scale_y_discrete(position = "right")
}
