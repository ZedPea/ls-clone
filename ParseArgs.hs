{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

module ParseArgs where

import System.Console.CmdArgs
import Prelude hiding (all)

ls :: LS
ls = LS { 
    file = def &= args &= typFile,
    all' = def &= explicit &= name "a" &= name "all" &= help "do not ignore entries starting with .",
    almost_all = def &= name "A" &= help "Do not list implied . and ..",
    author = def &= help "with -l, print the author of each file",
    escape = def &= name "b" &= help "print C-style escapes for nongraphic characters",
    block_size = def &= typ "SIZE" &= help "scale sizes by SIZE before printing them; e.g., '--block-size=M' prints sizes in units of 1,048,576 bytes; see SIZE format below",
    ignore_backups = def &= name "B" &= help "do not list implied entries ending with ~",
    list_by_columns = def &= explicit &= name "C" &= help "list entries by columns",
    nocolor = def &= explicit &= name "nocolor" &= help "disable colorisation of the output; enabled by default",
    directory = def &= help "list directories themselves, not their contents",
    dired = def &= name "D" &= name "dired" &= help "generate ouput designed for Emacs' dired mode",
    no_sort = def &= name "f" &= explicit &= help "do not sort, enable -aU, disable -ls --color",
    classify = def &= name "F" &= help "append indicator (one of */=>@|) to entries",
    filetype = def &= name "file-type" &= explicit &= help "likewise, except to not append '*'",
    format = def &= typ "WORD" &= help "across -x, commas -m, horizontal -x, long -l, single-column -1, verbose -l, vertical -C",
    full_time = def &= help "like -l --time-style=full-iso",
    long_no_owner = def &= name "g" &= explicit &= help "like -l, but do not list owner",
    group_dirs_first = def &= explicit &= name "group-directories-first" &= help "group directories before files;\n\ncan be augmented with a --sort option, but any use of --sort=none (-U) disables grouping",
    no_group = def &= name "no-group" &= name "G" &= help "in a long listing, don't print group names",
    human_readable = def &= name "human-readable" &= name "h" &= help "with -l and/or -s, print human readable sizes (e.g., 1K 234M 2G)",
    siUnits = def &= name "si" &= explicit &= help "likewise, but use powers of 1000 not 1024",
    follow_symLink = def &= explicit &= name "H" &= name "deref-cli" &= help "follow symbolic links listed on the command line",
    follow_dir_symLink = def &= explicit &= name "deref-cli-symlink-to-dir" &= help "follow each command line symbolic link\n\nthat points to a directory",
    hide = def &= typ "PATTERN" &= help "do not list implied entries matching shell PATTERN (over-ridden by -a or -A)",
    indicator_style = def &= typ "WORD" &= name "indicator-style" &= help "append indicator with style WORD to entry names: none (default), slash (-p), file-type (--file-type), classify (-F)",
    inode = def &= name "i" &= help "print the index number of each file",
    ignore' = def &= explicit &= name "ignore" &= typ "PATTERN" &= name "I" &= help "do not list implied entries matching shell PATTERN",
    kibibytes = def &= help "default to 1024-byte blocks for disk usage",
    long = def &= explicit &= name "l" &= help "use a long listing format",
    dereference = def &= name "L" &= name "dereference" &= help "when showing file information for a symbolic link, show information for the file the link references rather than for the link itself",
    comma_separated = def &= explicit &= name "m" &= help "fill width with a comma separated list of entries",
    num_UID_GID = def &= explicit &= name "n" &= name "numeric-uid-gid" &= help "like -l, but list numeric user and group IDs",
    literal = def &= name "N" &= name "literal" &= help "print raw entry names (don't treat e.g. control characters specially)",
    long_no_group = def &= name "o" &= explicit &= help "like -l, but do not list group information",
    add_slash = def &= explicit &= name "p" &= name "indicator-style=slash" &= help "append / indicator to directories",
    hide_control_chars = def &= name "q" &= name "hide-control-chars" &= help "print ? instead of nongraphic characters",
    show_control_chars = def &= name "show-control-chars" &= help "show nongraphic characters as-is (the default, unless program is 'ls' and output is a terminal)",
    quote_name = def &= name "Q" &= name "quote-name" &= help "enclose entry names in double quotes",
    quoting_style = def &= name "quoting-style" &= typ "WORD" &= help "use quoting style WORD for entry names: literal, locale, shell, shell-always, shell-escape, shell-escape-always, c, escape",
    reverse' = def &= explicit &= name "r" &= name "reverse" &= help "reverse order while sorting",
    recursive = def &= name "R" &= name "recursive" &= help "list subdirectories recursively",
    size = def &= name "s" &= help "print the allocated size of each file, in blocks",
    sort_by_size = def &= explicit &= name "S" &= help "sort by file size, largest first",
    sort = def &= name "sort" &= typ "WORD" &= help "sort by WORD instead of name: none (-U), size (-S), time (-t), version (-v_, extension (-X)",
    time = def &= name "time" &= typ "WORD" &= help "with -l, show time as WORD instead of default modification time: atime or access or use (-u); ctime or status (-c); also use specified time as sort key if --sort=time (newest first)",
    time_style = def &= name "time-style" &= typ "STYLE" &= help "with -l, show times using style STYLE: full-iso, long-iso, iso, locale, or +FORMAT; FORMAT is interpreted like in 'date'; if FORMAT is FORMAT1<newline>FORMAT2, then FORMAT1 applies to non-recent files and FORMAT2 to recent files; if STYLE is prefixed with 'posix-', STYLE takes effect only outside the POSIX locale",
    mod_time = def &= name "t" &= explicit &= help "sort by modification time, newest first",
    tab_size = def &= explicit &= name "T" &= name "tabsize" &= typ "COLS" &= help "assume tab stops at each COLS instead of 8",
    no_sort_dir_order = def &= name "U" &= explicit &= help "do not sort; list entries in directory order",
    sort_by_filename_versions = def &= name "v" &= explicit &= help "natural sort of (version) numbers withing text",
    width = def &= typ "COLS" &= help "set ouput width to COLS. 0 means no limit",
    lines' = def &= name "x" &= explicit &= help "list entries by lines instead of by columns",
    sort_by_extension = def &= explicit &= name "X" &= help "sort alphabetically by entry extension",
    security_context = def &= explicit &= name "Z" &= name "context" &= help "print any security context of each file",
    one_file_per_line = def &= name "1" &= explicit &= help "list one file per line. Avoid '\\n' with -q or -b"
} &= help "List information about the FILEs (the current directory by default).\nSort entries alphabetically if none of -cftuvSUX nor --sort is specified.\nMandatory arguments to long options are mandatory for short options too."
  &= summary ""
  &= details ["The SIZE argument is an integer and optional unit (example: 10K is 10*1024).","Units are K,M,G,T,P,E,Z,Y (powers of 1024) or KB,MB,...(powers of 1000).","","Using color to distinguish file types is enabled by default and is disabled with --nocolor. Color output is automatically disabled when not connected to a terminal."]

{-
need to use underscores so cmdargs converts them to dashes so it can
infer more argument names
-}
{-# ANN module "HLint: ignore Use camelCase" #-}
data LS = LS {
    file :: [FilePath],
    all' :: Bool,
    almost_all :: Bool,
    author :: Bool,
    escape :: Bool,
    block_size :: String, --maybe change to a custom size type later
    ignore_backups :: Bool,
    --I don't know what -c is really mean to do, will work out later
    list_by_columns :: Bool,
    nocolor :: Bool,
    directory :: Bool,
    dired :: Bool, --emacs fags yawn
    no_sort :: Bool,
    classify :: Bool,
    filetype :: Bool,
    format :: String, -- another custom type
    full_time :: Bool,
    long_no_owner :: Bool,
    group_dirs_first :: Bool,
    no_group :: Bool,
    human_readable :: Bool,
    siUnits :: Bool,
    follow_symLink :: Bool,
    follow_dir_symLink :: Bool,
    hide :: String, --another custom type
    indicator_style :: String, -- again...
    inode :: Bool,
    ignore' :: String, --regex
    kibibytes :: Bool,
    long :: Bool,
    dereference :: Bool,
    comma_separated :: Bool,
    num_UID_GID :: Bool,
    literal :: Bool,
    long_no_group :: Bool,
    add_slash :: Bool,
    hide_control_chars :: Bool,
    show_control_chars :: Bool, --default?
    quote_name :: Bool,
    quoting_style :: String, --custom type
    reverse' :: Bool,
    recursive :: Bool,
    size :: Bool,
    sort_by_size :: Bool,
    sort :: String, -- custom
    time :: String, --custom
    time_style :: String, --custom
    mod_time :: Bool,
    tab_size :: String, --we could maybe make some of these just an int
    --I don't know what -u is meant to do
    no_sort_dir_order :: Bool,
    sort_by_filename_versions :: Bool,
    width :: String, --custom
    lines' :: Bool,
    sort_by_extension :: Bool,
    security_context :: Bool,
    one_file_per_line :: Bool
} deriving (Data,Typeable,Show)
