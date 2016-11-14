# ls-clone
A clone of GNU ls

Usage: ./ls-clone [OPTION]... [FILE]...

Currently supported flags:

-? / --help

-a / --all

-A / --almost-all

-R / --recursive

--nocolor

Abandoned due to it being really boilerplatey and boring to add support for all the edge cases on printing.

For example, if passes a directory on cli, if it's empty, nothing should be printed. However, if passed two or more directories, even if empty, we print dir: then a newline for each. This means we can't know which functions to call before listing the directory, which just makes for big ugly functions which aren't easy to map over.
