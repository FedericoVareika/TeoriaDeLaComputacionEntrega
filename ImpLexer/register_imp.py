from pygments.lexers import _mapping
from pygments.lexers import get_all_lexers
from pygments.lexers import load_lexer_from_file
from pygments.lexers import ImpLexer

_mapping.LEXERS['Imp'] = ('imp_lexer', 'Imp', ('imp',), ('*.imp',), ('text/x-imp',))

