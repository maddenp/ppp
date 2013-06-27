$: << (basedir=File.dirname($0)) << File.join(basedir,"lib")

# ruby requires

require "fileutils"
require "ostruct"
require "pathname"
require "set"
require "socket"
require "thread"
require "yaml"

# ppp requires

require "treetop/runtime"

require "normfixed"
require "normfixed_parser"
require "sms_normfixed_parser"
require "sms_normfixed"

require "normfree"
require "normfree_parser"
require "sms_normfree_parser"
require "sms_normfree"

require "fortran"
require "fortran_parser"
require "sms_fortran_parser"
require "sms_fortran"

require "translator"

include Translator
