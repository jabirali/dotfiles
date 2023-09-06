vim9script

g:wiki_root = '/Users/jabirali/iCloud/Notes'
g:wiki_zotero_root = '/Users/jabirali/Zotero'
g:wiki_filetypes = ['md']
g:wiki_link_extension = '.md'
g:wiki_mappings_local_journal = {
	'<plug>(wiki-journal-prev)': '[f',
	'<plug>(wiki-journal-next)': ']f',
}
g:wiki_journal = {
	'name': '',
	'frequency': 'daily',
	'date_format': {
			'daily': '%Y-%m-%d',
			'weekly': '%Y-W%W',
	},
}
