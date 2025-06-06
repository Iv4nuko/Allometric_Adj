#estandarizacion alometrica Lleonart et al (2000)
normalizar_medida <- function(data, Y_name, X_name, X0 = NULL, b = NULL) {
  Y <- data[[Y_name]]
  X <- data[[X_name]]
  
  if (is.null(X0)) {
    X0 <- mean(X, na.rm = TRUE)
  }
  
  if (is.null(b)) {
    b <- coef(lm(log(Y) ~ log(X)))[2]  # Estima el exponente alométrico
    modelo=lm(log(Y) ~ log(X))
    print(summary(modelo))
  }
  
  Y_norm <- Y * (X0 / X)^b
  data[[paste0(Y_name, ".std")]] <- Y_norm
  
  return(data)
}
proceso=function(data,range,X,group_name){
if (!require("dplyr")) {
  install.packages("dplyr")
  library(dplyr)}
  var_to_normallo=colnames(data)[range]
for (var in var_to_normallo){
  data=
    data %>% group_by(group_name)%>%
    group_modify(~normalizar_medida(.x,Y_name = var,X_name = X)) %>%
    ungroup()}
  }
