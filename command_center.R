

while (T) {
  
  rstudioapi::jobRunScript("checker.R")
  Sys.sleep(60*5)
}