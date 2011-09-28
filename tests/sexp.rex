%option case-insensitive

name-char     => \d|\w|_|-|\+|=|\*|&|\^|%|\$|@|!

%%

prefix        =>  `|'|#
name          =>  (?:{name-char})+
atom          =>  (?:(?<string>)|(?<symbol-bars>)|(?<name>))
sexp-list     =>  \s*(?:(?<sexp>)\s+)*(?<sexp>)\s*
less-sexp     =>  (?<parens>(?<sexp-list>)?)|(?<atom>)
sexp          =>  (?<prefix>)(?<sexp>)|(?<less-sexp>)