setwd("/bioverbs")
rcmdcheck::rcmdcheck(path = ".", args = "--no-manual")
BiocCheck::BiocCheck(package = ".")
