import ply.lex as lex
import ply.yacc as yacc
import sys

reserved = {
	'and' : 'AND',
	'or' : 'OR',
	'not' : 'NOT',
	'in' : 'IN',
	'while' : 'WHILE',
	'if' : 'IF',
	'else' : 'ELSE',
	'print' : 'PRINT',
	'return' : 'RETURN'
};

tokens = ['LPAREN', 
	'RPAREN', 
	'PLUS', 
	'MINUS', 
	'MULT', 
	'DIVIDE',
	'INT', 
	'REAL', 
	'COMP_GT', 
	'COMP_LT', 
	'COMP_EQ', 
	'COMP_GTE', 
	'COMP_LTE', 
	'COMP_NEQ', 
	'FLOOR_DIV', 
	'EXPONENT',
	'MOD',
	'OR',
	'AND',
	'NOT',
	'IN',
	'STRING',
	'COMMA',
	'LBRAC',
	'RBRAC',
	'VARNAME',
	'EQUALS',
	'LCBRACE',
	'RCBRACE',
	'SEMI',
	'PRINT',
	'WHILE',
	'IF',
	'ELSE',
	'RETURN'
	]

t_ignore = ' \t'
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULT = r'\*'
t_DIVIDE = r'\/'
t_EQUALS = r'\='
t_COMP_GT = r'\>'
t_COMP_LT = r'\<'
t_COMP_EQ = r'\=\='
t_COMP_GTE = r'\>\='
t_COMP_LTE = r'\<\='
t_COMP_NEQ = r'\<\>'
t_FLOOR_DIV = r'\/\/'
t_EXPONENT = r'\*\*'
t_MOD = r'\%'
t_COMMA = r'\,'
t_LBRAC = r'\['
t_RBRAC = r'\]'
t_LCBRACE = r'\{'
t_RCBRACE = r'\}'
t_SEMI = r'\;'

def t_ID(t):
	r'[a-zA-Z][A-Za-z0-9_]*'
	if t.value in reserved:
		t.type = reserved.get(t.value, 'ID')
		return t
	else:
		return t_VARNAME(t)

def t_REAL(t):
	r'\d+\.\d+'
	t.value = float(t.value)
	return t

def t_INT(t):
	r'\d+'
	t.value = int(t.value)
	return t

def t_STRING(t):
	r'\".*?\"'
	t.value = str(t.value)
	return t

def t_newline(t):		# define a rule so we can track line numbers
	r'\n+'
	t.lexer.lineno += len(t.value)

def t_VARNAME(t):
	r'[a-zA-Z][A-Za-z0-9_]*'
	t.type = 'VARNAME'
	return t

def t_error(t):			# a string containing ignored characters (spaces & tabs)
	t.lexer.skip(1)

lexer = lex.lex()

precedence = (

	('left', 'RETURN'),
	('left', 'EQUALS'),
	('left', 'OR'),
	('left', 'AND'),
	('left', 'NOT'),
	('left', 'COMP_GT', 'COMP_LT', 'COMP_EQ', 'COMP_GTE', 'COMP_LTE', 'COMP_NEQ'),
	('left', 'IN'),
	('left', 'PLUS', 'MINUS'),
	('left', 'FLOOR_DIV'),
	('left', 'EXPONENT'),
	('left', 'MOD'),
	('left', 'MULT', 'DIVIDE'),
	('left', 'LBRAC', 'RBRAC'),
	('right', 'UMINUS')

)

class CharlesStack:
	def __init__(self):
		self.items = []
	def isEmpty(self):
		return self.items == []
	def push(self, item):
		self.items.append(item)
	def pop(self):
		return self.items.pop()
	def peek(self):
		return self.items[len(self.items)-1]
	def size(self):
		return len(self.items)

methods = dict()
global_stack = CharlesStack()
static_scope = dict()
global_stack.push(static_scope)

class Node:
    def __init__(self):
        self.x = 0
    def evaluate(self):
        return 0
    def execute(self): 
        self.x = 0

class MainNode(Node):
	def __init__(self, body):
		self.body = body
	def evaluate(self):
		for i in self.body:
			i.evaluate()
	def execute(self):
		return self.evaluate() 

class MethodNode(Node):
	def __init__(self, mname, params, mblock):
		self.mname = mname
		self.params = params
		self.mblock = mblock
	def evaluate(self):
		self.execute()
	def execute(self):
		methods[self.mname] = (self.params, self.mblock)

class MethodCallNode(Node):
	def __init__(self, mname, params):
		self.mname = mname
		self.params = params
	def evaluate(self):
		return self.execute()
	def execute(self):
		thisScope = dict()
		meth = self.mname
		if meth in methods:
			dalist = methods[meth][0]
			if len(dalist) == len(self.params):
				for i in range(len(dalist)):
					thisScope[dalist[i].vname] = self.params[i].evaluate()
				global_stack.push(thisScope)
				methods[meth][1].popScope = True
				x = methods[meth][1].execute()
				return x
			else:
				raise ValueError("SEMANTIC ERROR")
		else:
			raise ValueError("SEMANTIC ERROR")

class VarNameNode(Node):
	def __init__(self, vname):
		self.vname = vname
	def evaluate(self):
		scope = global_stack.peek()
		if self.vname in scope:
			return global_stack.peek()[self.vname]
		else:
			if self.vname in static_scope:
				return static_scope[self.vname]
			else:
				raise ValueError("SEMANTIC ERROR")
	def execute(self):
		return self.evaluate()

class IntNode(Node):
	def __init__(self, v):
		self.value = int(v)
	def evaluate(self):
		return self.value
	def execute(self):
		return self.value   

class RealNode(Node):
	def __init__(self, v):
		self.value = float(v)
	def evaluate(self):
		return self.value
	def execute(self):
		return self.value
 
class StringNode(Node):
    def __init__(self, v):
        self.value = str(v)
        self.value = self.value[1:-1] # to eliminate the left and right double quotes
    def evaluate(self):
        return self.value
    def execute(self):
        return self.value
 
class PrintNode(Node):
    def __init__(self, v):
        self.value = v
    def evaluate(self):
        self.execute()
    def execute(self):
        print(self.value.evaluate())
 
class IfNode(Node):
    def __init__(self, c, t, e):
        self.condition = c
        self.thenBlock = t
        self.elseBlock= e
    def evaluate(self):
        return 0
    def execute(self):
        try:
        	if(self.condition.evaluate()):
        		self.thenBlock.popScope = False
        		self.thenBlock.execute()
	        else:
	        	self.elseBlock.popScope = False
	        	self.elseBlock.execute()
        except:
        	raise ValueError("SEMANTIC ERROR")

class IndexNode(Node):
	def __init__(self, name, ind):
		self.name = name
		self.ind = ind
	def evaluate(self):
		return self.name.evaluate()[self.ind.evaluate()]
	def execute(self):
		self.evaluate()
	def indexing(self, value):
		left = self.name.evaluate()
		right = self.ind.evaluate()
		left[right] = value.evaluate()

class ListNode(Node):
	def __init__(self, l):
		self.list = l
	def evaluate(self):
		ctr = 0
		for i in self.list:
			self.list[ctr] = i.evaluate()
			ctr = ctr + 1
		return self.list
	def execute(self):
		return self.evaluate()		
 
class BlockNode(Node):
    def __init__(self, sl):
        self.statementNodes = sl
        self.popScope = False
        self.newScope = False
    def evaluate(self):
        return self.execute()
    def execute(self):
    	x = None
    	if self.newScope == True:
    		global_stack.push(dict())
    	try:
        	for statement in self.statementNodes:
        		if isinstance(statement, ReturnNode):
        			x = statement.execute()
        			if self.popScope == True:
        				global_stack.pop()
        			return x
        		else:
        			x = statement.execute()
        			if(x != None):
        				if self.popScope == True:
        					global_stack.pop()
        				return x
        	if self.popScope == True:
        		global_stack.pop()
        	return x
    	except:
    		raise ValueError("HISEMANTIC ERROR")

class ReturnNode(Node):
	def __init__(self, val):
		self.val = val
	def evaluate(self):
		return self.execute()
	def execute(self):
		return self.val.execute()

class AssignmentNode(Node):
	def __init__(self, var, val):
		self.var = var
		self.val = val
	def evaluate(self):
		self.execute()
	def execute(self):
		if isinstance(self.var, IndexNode):
			scope = global_stack.peek()
			if self.var in scope:
				global_stack.peek()[self.var.indexing(self.val)] = self.val.evaluate()
			else:
				static_scope[self.var.indexing(self.val)] = self.val.evaluate()
		else:
			global_stack.peek()[self.var.vname] = self.val.evaluate()

class ExpressionNode(Node):
	def __init__(self, op, v1, v2):
		self.op = op
		self.v1 = v1
		self.v2 = v2
	def evaluate(self):
		return self.execute()
	def execute(self):
		l = self.v1.evaluate()
		r = self.v2.evaluate()
		if self.op == '+':
			return l+r
		elif self.op == '-':
			return l-r
		elif self.op == '*':
			return l*r
		elif self.op == '/':
			return l/r
		elif self.op == '//':
			return l//r
		elif self.op == '**':
			return l**r
		elif self.op == '%':
			return l%r
		elif self.op == 'not':
			if isinstance(l, int):
				if (not l) == False: 
					return 0
				else: 
					return 1
			else:
				raise ValueError("SEMANTIC ERROR")
		elif self.op == 'and':
			if isinstance(l, int) and isinstance(l, int):
				if (l and r) != 0:
					return 1
				else:
					return 0
			else:
				raise ValueError("SEMANTIC ERROR")
		elif self.op == 'in':
			if (isinstance(l, int) and isinstance(r, int)) or (isinstance(l, list) and isinstance(r, list)) or (isinstance(l, int) and isinstance(r, list)):
				if (l in r) != 0:
					return 1
				else:
					return 0
			else:
				raise ValueError("SEMANTIC ERROR")
		elif self.op == '>':
			if isinstance(l, int) and isinstance(r, int):
				if (l > r) == False:
					return 0
				else:
					return 1
			else:
				raise ValueError("SEMANTIC ERROR")
		elif self.op == '<':
			if isinstance(l, int) and isinstance(r, int):
				if (l < r) == False:
					return 0
				else:
					return 1
			else:
				raise ValueError("SEMANTIC ERROR")
		elif self.op == '<=':
			if isinstance(l, int) and isinstance(r, int):
				if (l <= r) == False:
					return 0
				else:
					return 1
			else:
				raise ValueError("SEMANTIC ERROR")
		elif self.op == '>=':
			if isinstance(l, int) and isinstance(r, int):
				if (l >= r) == False:
					return 0
				else:
					return 1
			else:
				raise ValueError("SEMANTIC ERROR")
		elif self.op == '==':
			if isinstance(l, int) and isinstance(r, int):
				if (l == r) == False:
					return 0
				else:
					return 1
			else:
				raise ValueError("SEMANTIC ERROR")
		elif self.op == '<>':
			if isinstance(l, int) and isinstance(r, int):
				if (l != r) == False:
					return 0
				else:
					return 1
			else:
				raise ValueError("SEMANTIC ERROR")
		else:
			print("UNKNOWN ERROR")

class WhileNode(Node):
	def __init__(self, cond, block):
		self.condition = cond
		self.block = block
	def evaluate(self):
		return 0
	def execute(self):
		try:
			while(self.condition.evaluate()):
				self.block.execute()
		except:
			raise ValueError("SEMANTIC ERROR")

def p_program(p):
	"""
	program : program_tl
	"""
	p[0] = MainNode(p[1])

def p_program_block(p):
	"""
	program_tl : block program_tl
	"""
	p[1].popScope = True
	p[1].newScope = True
	p[0] = [p[1]] + p[2]

def p_program_tl(p):
	"""
	program_tl : empty
	"""
	p[0] = []

def p_return(p):
	"""
	expression : RETURN expression
	"""
	p[0] = ReturnNode(p[2])

def p_program_assign(p):
	"""
	program_tl : assignment SEMI program_tl
	"""
	p[0] = [p[1]] + p[3]

def p_block(p):
	"""
	block : LCBRACE stmt_list RCBRACE
	"""
	p[0] = BlockNode(p[2])

def p_param(p):
	"""
	parameters : expression param_tl
	"""
	p[0] = [p[1]] + p[2]

def p_param_tl(p):
	"""
	param_tl : COMMA expression param_tl
	"""
	p[0] = [p[2]] + p[3]

def p_param_e(p):
	"""
	param_tl : 
	"""
	p[0] = []

def p_func_exp(p):
	"""
	program_tl : function program_tl
	"""
	p[0] = [p[1]] + p[2]

def p_func(p):
	"""
	function : VARNAME LPAREN parameters RPAREN block
	"""
	p[0] = MethodNode(p[1], p[3], p[5])

def p_func_call_exp(p):
	"""
	expression : function_call
	"""
	p[0] = p[1]

def p_func_call(p):
	"""
	function_call : VARNAME LPAREN parameters RPAREN
	"""
	p[0] = MethodCallNode(p[1], p[3])

def p_stmt_list(p):
	"""
	stmt_list : stmt SEMI stmt_cc 
	"""
	p[0] = [p[1]] + p[3]

def p_stmt_list2(p):
	"""
	stmt_list : conditional stmt_cc
	"""
	p[0] = [p[1]] + p[2]

def p_stmt(p):
	"""
	stmt : expression
	"""
	p[0] = p[1]

def p_stmt_cc(p):
	"""
	stmt_cc : expression SEMI stmt_cc
	"""
	p[0] = [p[1]] + p[3]

def p_stmt_cc_e(p):
	"""
	stmt_cc : 
	"""
	p[0] = []

def p_stmt_cc2(p):
	"""
	stmt_cc : conditional stmt_cc
	"""
	p[0] = [p[1]] + p[2]

def p_conditional_wh(p):
	"""
	conditional : wh_cond wh_blk
	"""
	p[0] = WhileNode(p[1], p[2])

def p_cond_wh(p):
	"""
	wh_cond : WHILE LPAREN expression RPAREN
	"""
	p[0] = p[3]

def p_wh_blk(p):
	"""
	wh_blk : LCBRACE stmt_list RCBRACE
	"""
	p[0] = BlockNode(p[2])

def p_cond_if(p):
	"""
	if_cond : IF LPAREN expression RPAREN
	"""
	p[0] = p[3]

def p_if_block(p):
	"""
	if_block : LCBRACE stmt_list RCBRACE
	"""
	p[0] = BlockNode(p[2])

def p_else_block(p):
	"""
	else_block : 
	"""
	p[0] = Node()

def p_else_block2(p):
	"""
	else_block : ELSE LCBRACE stmt_list RCBRACE
	"""
	p[0] = BlockNode(p[3])

def p_conditional_if(p):
	"""
	conditional : if_cond if_block else_block
	"""
	p[0] = IfNode(p[1], p[2], p[3])

def p_empty(p):
	"""
	empty : 
	"""
	p[0] = None

def p_asssign(p):
	"""
	expression : assignment
	"""
	p[0] = p[1]

def p_assignment(p):
	"""
	assignment : expression EQUALS expression
	"""
	p[0] = AssignmentNode(p[1], p[3])

def p_assign_name(p):
	"""
	expression : VARNAME
	"""
	p[0] = VarNameNode(p[1])

def p_expression(p):
	"""
	expression : expression MULT expression
				| expression DIVIDE expression
				| expression PLUS expression
				| expression MINUS expression
				| expression COMP_GT expression
				| expression COMP_LT expression
				| expression COMP_EQ expression
				| expression COMP_GTE expression
				| expression COMP_LTE expression
				| expression COMP_NEQ expression
				| expression FLOOR_DIV expression
				| expression EXPONENT expression
				| expression MOD expression
				| expression AND expression
				| expression OR expression
				| expression IN expression
	"""
	p[0] = ExpressionNode(p[2], p[1], p[3])

def p_expression_not(p):
	"""
	expression : NOT expression
	"""
	p[0] = ExpressionNode(p[1], p[2], Node())

def p_expression_int(p):
	"""
	expression : INT
	"""
	p[0] = IntNode(p[1])

def p_expression_real(p):
	"""
	expression : REAL
	"""
	p[0] = RealNode(p[1])

def p_expression_string(p):
	"""
	expression : STRING
	"""
	p[0] = StringNode(p[1])

def p_expression_list(p):
	"""
	expression : list
	"""
	p[0] = ListNode(p[1])

def p_expression_paren(p):
	"""
	expression : LPAREN expression RPAREN
	"""
	p[0] = p[2]

def p_expression_uminus(p):
	"""
	expression : MINUS expression %prec UMINUS
	"""
	p[0] = -run(p[2])

def p_indexCount(p):
	"""
	expression : expression LBRAC expression RBRAC
	"""
	p[0] = IndexNode(p[1], p[3])

def p_list(p):
	"""
	list : LBRAC innerlist RBRAC
	"""
	p[0] = p[2]

def p_list2(p):
	"""
	list : LBRAC expression RBRAC
	"""
	p[0] = [p[2]]

def p_innerlist(p):
	"""
	innerlist : expression COMMA expression
	"""
	p[0] = [p[1]] + [p[3]]

def p_innerlist2(p):
	""" 
	innerlist : innerlist COMMA expression
	"""
	p[0] = p[1] + [p[3]]

def p_statement_print(p):
    """ 
    expression : PRINT LPAREN expression RPAREN
    """
    p[0] = PrintNode(p[3]) # create nodes in the tree instead of executing the current expression

def p_error(p):
	print(p)
	print("SYNTAX ERROR")

parser = yacc.yacc()
file = open(sys.argv[1], "r")
ast = parser.parse(file.read())

ast.execute();
