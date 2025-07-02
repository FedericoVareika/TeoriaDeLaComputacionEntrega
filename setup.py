from setuptools import setup

setup(
    name='pygments-imp',
    packages=['ImpLexer'],
    entry_points={
        'pygments.lexers': ['imp = ImpLexer.imp_lexer:ImpLexer'],
    },
)

