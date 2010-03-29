if exists("did_load_filetypes")
      finish
endif

augroup filetypedetect
      au BufRead,BufNewFile *.mkd                     setfiletype mkd
      au BufRead,BufNewFile *.wikipedia               setfiletype Wikipedia
      au BufRead,BufNewFile *.clj                     setfiletype clojure
      au BufRead,BufNewFile *.rem                     setfiletype remind
      au BufRead,BufNewFile README*,NOTES*,TODO*      setfiletype text
augroup END
