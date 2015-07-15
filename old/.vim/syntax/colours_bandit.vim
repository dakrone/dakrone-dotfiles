syn clear

" Highlight colors/bandit.vim in the bandit.vim colour scheme!

runtime! syntax/vim.vim

redir => highlights
silent hi
redir END

let lines = split(highlights, '\n')
let keywords = []
for line in lines
	let keyword = split(line)[0]
	let keywords += [keyword,]
endfor

syn clear vimVar

for keyword in keywords
	execute 'syn match '.keyword." /ColourAssignment\\['".keyword."'\\]/me=s+16"
endfor


let b:current_syntax = "colours_bandit"
