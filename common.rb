$: << (basedir=File.dirname($0)) << File.join(basedir,"lib")

require "treetop/runtime"
require "sms_normfixed"
require "sms_normfree"
require "sms_fortran"
require "translator"

include Translator
