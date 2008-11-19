" ****************************************************************************
" File:             narrow.vim
" Author:           Jonas Kramer
" Version:          0.1
" Last Modified:    2008-11-17
" Copyright:        Copyright (C) 2008 by Jonas Kramer. Published under the
"                   terms of the Artistic License 2.0.
" ****************************************************************************
" Installation: Copy this script into your plugin folder.
" Usage: Call the command :Narrow with a range to zoom into the range area.
" Call :Widen to zoom out again.
" WARNING! Be careful when doing undo operations in a narrowed buffer. If you
" undo the :Narrow action, :Widen will fail miserable and you'll probably have
" the hidden parts doubled in your buffer. The 'u' key is remapped to a save
" undo function, but you can still mess this plugin up with :earlier, g- etc.
" Also make sure that you don't mess with the buffers autocmd BufWriteCmd
" hook, as it is set to a function that allows saving of the whole buffer
" instead of only the narrowed region in narrowed mode. Otherwise, when saving
" in a narrowed buffer, only the region you zoomed into would be saved.
" ****************************************************************************

let s:NarrowP = {}


fu! narrow#Narrow(rb, re)
	let n = bufnr("%")

	if has_key(s:NarrowP, n)
		echo "Buffer is already narrowed. Widen first, then select a new region."
	else
		" Save modified state.
		let modified = getbufvar(n, "&modified")

		let prr = getline(1, a:rb - 1)
		let por = getline(a:re + 1, "$")
		let s:NarrowP[n] = { "pre": prr, "post": por, "rb": a:rb, "re": a:re }

		exe "silent " . (a:re + 1) . ",$d"
		exe "silent 1," . (a:rb - 1) . "d"

		let s:NarrowP[n]["ch"] = changenr()

		au BufWriteCmd <buffer> call narrow#Save()

		" If buffer wasn't modify, unset modified flag.
		if !modified
			set nomodified
		en

		echo "Narrowed. Be careful with undo/time travelling. " . changenr()
	endi
endf


fu! narrow#Widen()
	let n = bufnr("%")

	if has_key(s:NarrowP, n)
		" Save modified state.
		let modified = getbufvar(n, "&modified")

		let text = remove(s:NarrowP, n)

		let content = copy(text["pre"])
		let content = extend(content, copy(getline(1, "$")))
		let content = extend(content, copy(text["post"]))

		call setline(1, content)

		au! BufWriteCmd <buffer>

		" If buffer wasn't modify, unset modified flag.
		if !modified
			set nomodified
		en

		echo "Widened. " . changenr()
	endi
endf


fu! narrow#Save()
	let n = bufnr("%")
	let name = bufname("%")

	if has_key(s:NarrowP, n)
		let text = s:NarrowP[n]

		let content = copy(text["pre"])
		let content = extend(content, copy(getline(1, "$")))
		let content = extend(content, copy(text["post"]))

		call writefile(content, name)
		set nomodified
		echo "Saved something, not sure if it worked."
	endi
endf


fu! narrow#SaveUndo()
	let n = bufnr("%")

	if has_key(s:NarrowP, n)
		let pos = getpos(".")

		silent undo
		if changenr() < s:NarrowP[n]["ch"]
			silent redo
			echo "I said, be careful with undo! Widen first. " . changenr()
			call setpos(".", pos)
		en
	else
		undo
	en
endf


command! -bar -range Narrow call narrow#Narrow(<line1>, <line2>)
command! -bar Widen call narrow#Widen()

map u :call narrow#SaveUndo()<Cr>
