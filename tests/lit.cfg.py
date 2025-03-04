import os

import lit.util
import lit.formats

config.name = "LLVM"

config.test_format = lit.formats.ShTest()

config.suffixes = [".ll", ".c", ".test", ".yaml"]

# test_source_root: The root path where tests are located.
config.test_source_root = os.path.dirname(__file__)
