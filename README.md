jwasm
=====

Fork of JWasm by http://www.japheth.de with customizations. See `Readme.txt`
for details on the original JWasm.

These customizations are mostly aimed at syncing JWasm and MASM preprocessors'
behavior and making it possible to easily use JWasm preprocessor instead of
MASM on platforms other than Windows.

OS X compatibility
------------------

This fork can be built on OS X. To do that simply run

    $ make -f GccOSX.mak

Or

    $ make -f GccOSX.mak DEBUG=1

to build a version with debug features.

**NOTE:** If you're running Xcode 4.3 and newer, you need Command Line Tools
in order to build this project.

MASM -EP compatibility
----------------------

This fork was fixed to ignore any assembly passes other than the preprocessor
pass when `-EP` option was given. Original JWasm would go forward and try to
create the object file which is not what MASM does.

Also `.err` file generation was suppressed for `-EP` since this option usually
implies feeding the preprocessed code through some pipe and additional files are
not wanted and even may do bad.

If you use `-eq` option with `-EP` then JWasm will behave as a _totally
forgiving_ preprocessor. This means that while no errors are reported, JWasm
return code will also be `0`. That's because if you suppress errors on screen
and do not write them to `.err` file, there's no good in seeing the build fail
with no error messages.
