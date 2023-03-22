##' Convert UTF-16LE files to UTF-8
#'
#' VAN changed its default export format. Boo!
#'
#' @name convert_utf
#' @keywords convert utf utf-16 utf-16le
#' @export convert_utf
#'
#' @usage convert_utf(infile, outfile, python_cmd="py")
#'
#' @param infile The path to the file to load (already extracted from zip).
#' @param outfile The name of the converted file to write (cannot be the same as infile).
#' @param python_cmd The path you use to get to python on your system's command line---common values are \code{py}, \code{python}, or \code{python3}.


# this is a completely wild approach, I acknowledge, but maybe it'll work:

pycode <- c(
  "import sys",
  "",
  "def convert_utf(infile:str, outfile:str):",
  "  with open(infile, mode='r', encoding='utf-16le') as inf, open(outfile, mode='w', encoding='utf-8') as outf:",
  "    for line in inf:",
  "      outf.write(line)",
  "  print(f\"Converted {infile} to {outfile}.\")",
  "  return",
  "",
  "if __name__ == \"__main__\":",
  "  if len(sys.argv) < 2:",
  "    sys.exit(f\"You provided arguments {str(sys.argv)}. Usage: provide both infilename and outfilename.\")",
  "  else:",
  "    print(f\"You provided arguments {str(sys.argv)}.\")",
  "    convert_utf(infile=sys.argv[1], outfile=sys.argv[2])"
)

# TODOs:

# delete the original file?
# file.unlink()

# unzip?
# import zipfile
# with zipfile.ZipFile(zipped_file, 'r') as zip_ref:
#   zip_ref.extractall(today_folder)

# rezip?


convert_utf <- function(infile, outfile, python_cmd = "py") {
  # on first run, create the python code in working directory........
  if(!file.exists("convert_utf.py")) {
    writeLines(pycode, "convert_utf.py")
  }
  # run command:
  system2(
    command = python_cmd, 
    args = c("convert_utf.py", shQuote(infile), shQuote(outfile))
  )
  return(outfile)
}

# convert_utf(
#   infile = 'WAStdTxtWarehouseUpdate20221007-18932312115.txt',
#   outfile = 'WAStdTxt.txt'
# )