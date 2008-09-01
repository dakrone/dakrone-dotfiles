if exists("did_load_filetypes")
      finish
endif

augroup filetypedetect
      au BufRead,BufNewFile *.mkd                     setfiletype mkd
      au BufRead,BufNewFile README*,NOTES*,TODO*      setfiletype text
augroup END
