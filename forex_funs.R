library(magrittr)

get_folder_paths = function() {
  paths = list.files(path="Forex/", pattern=NULL, all.files=FALSE, full.names=TRUE)
  return(paths)
}

get_csv_paths = function(folder) {
  csv_paths = list.files(path=folder, pattern=".csv", all.files=FALSE, full.names=TRUE)
  csv_paths = sort(csv_paths)
  
  matches = grepl("combined", csv_paths)
  csv_paths = csv_paths[!matches]
  
  return(csv_paths)
}

combine_paths = function(csv_paths, save=TRUE) {
  df = NULL
  counter = 0
  for (path in csv_paths) {
    if (is.null(df)) {
      df = read.csv2(file=path, header=FALSE) %>%
        dplyr::mutate(date = strptime(V1, format="%Y%m%d %H%M%S")) %>%
        dplyr::rename(price = V2) %>%
        dplyr::select(date, price)
      counter = counter + nrow(df)
      print(paste0(path, ": ", nrow(df)))
    } else {
      other_df = read.csv2(file=path, header=FALSE) %>%
        dplyr::mutate(date = strptime(V1, format="%Y%m%d %H%M%S")) %>%
        dplyr::rename(price = V2) %>%
        dplyr::select(date, price)
      counter = counter + nrow(other_df)
      print(paste0(path, ": ", nrow(other_df))) 
      df = rbind(df, other_df)
    }
  }
  print(paste0("Actual:", nrow(df)))
  print(paste0("Expected: ", counter))
  return(df)
}

save_df = function(folder, name, df) {
  print("Saving...")
  save_name = paste0(folder, "/", name, "_combined_df.csv")
  write.csv(df, save_name)
  print("Saved!")
}

combiner = function(names=NULL, save=TRUE) {
  if (is.null(names)) {
    folder_paths = get_folder_paths()
  } else {
    folder_paths = c()
    for (i in 1:length(names)) {
      folder_paths[i] = paste0("Forex/", toupper(names[i]), "/")
    }
  }
  
  for (folder in folder_paths) {
    csv_paths = get_csv_paths(folder)
    df = combine_paths(csv_paths)
    
    if (isTRUE(save)) {
      name = strsplit(folder, split="/")[[1]][2]
      save_df(folder, name, df)
    }
  }
}

main_combiner = function(names, save) {
  if (is.null(names)) {
    folder_paths = get_folder_paths()
  } else {
    folder_paths = c()
    for (i in 1:length(names)) {
      folder_paths[i] = paste0("Forex/", toupper(names[i]), "/")
    }
  }
  
  for (folder in folder_paths) {
    name = paste0(strsplit(folder, split="/")[[1]][2], "_combined_df.csv")
  }
}

combine_loader = function(list=TRUE) {
  folder_paths = get_folder_paths()
  
  for (folder in folder_paths) {
    name = substring(folder, first=7)
    file_name = paste0(folder, "/", name, "_combined_df.csv")
    df = read.csv(file_name, header=TRUE)
  }
}

combiner(save=TRUE)
