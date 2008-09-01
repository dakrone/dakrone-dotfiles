" process only once
if exists("b:did_ftplugin") || &compatible
      finish
endif
let b:did_ftplugin = 1

setlocal noexpandtab


