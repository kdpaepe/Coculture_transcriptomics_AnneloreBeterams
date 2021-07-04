# 2021 from https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode

getmode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}